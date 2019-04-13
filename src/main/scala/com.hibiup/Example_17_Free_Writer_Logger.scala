package com.hibiup

package Example_17_Free_Kleisli_State_Logger {
    import cats.data._
    import cats.effect.IO
    import cats.free.Free
    import cats.{Id, Monad, ~>}
    import cats.implicits._
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory

    import scala.concurrent.duration.Duration
    import scala.concurrent.{Await, Future}

    object FreeWriterLogger extends App{
        trait ADTs {
            /***************************************
              * 1) ADT
              * */
            trait Result[+A]
            case class I2F(i:Int) extends Result[BigDecimal]
            case class F2S(f:BigDecimal) extends Result[String]
            case class F2B(f:BigDecimal) extends Result[String]

            /****************************************
              * 2) Lift
              * */
            type ResultF[A] = Free[Result, A]
        }

        object ADTs extends ADTs {
            type Report = Vector[IO[Unit]]
            type λ[α] = WriterT[Future, Report, α]
            type ResultT[A] = EitherT[λ, Throwable, A]
        }


        /**************************************
          * 3) 定义 DSL (代数)
          * */
        object Alg {
            import ADTs._

            def i2f(i:Int):ResultF[BigDecimal] = Free.liftF[Result, BigDecimal](I2F(i))
            def f2s(f:BigDecimal):ResultF[String] = Free.liftF[Result, String](F2S(f))
            def f2b(f:BigDecimal):ResultF[String] = Free.liftF[Result, String](F2B(f))

            /* Free 组合 */
            def comp(i:Int):ResultF[String] = for {
                f <- i2f(i)
                s <- if (f>1) f2s(f) else f2b(f)   /** 在 for-comprehension 里实现逻辑分支. */
            } yield s
        }


        /***********************************************
          * 4) Free router
          *
          * 业务逻辑解释器
          * */
        trait Compiler[M[_]] {
            import ADTs._
            // 将一个 Free Monad 映射到一个带有业务运算的函数
            // 需要一个 Monad 实现 ResultT 数据类型的转换
            def apply[A](action: ResultF[A])(implicit monad:Monad[M]): M[A] = action.foldMap(route)

            private val route: Result ~> M = new (Result ~> M) {
                override def apply[A](fa: Result[A]): M[A] = fa match {
                    case I2F(i) => i2f(i).asInstanceOf[M[A]]
                    case F2S(f) => f2s(f).asInstanceOf[M[A]]
                    case F2B(f) => f2b(f).asInstanceOf[M[A]]
                }
            }

            def i2f(i:Int): ResultT[BigDecimal]
            def f2s(f:BigDecimal): ResultT[String]
            def f2b(f:BigDecimal): ResultT[String]
        }


        /****************************************************
          * 业务实现
          *
          * 辅助 ResultT 运算的 Monad. Cats 和 Scalaz 提供了大部分基本的数据类型的 Monad，因此大部分情况下不需要自己实现．
          * */
        object Implement {
            import ADTs.ResultT

            val logger = Logger(LoggerFactory.getLogger(this.getClass))
            implicit val ec =  scala.concurrent.ExecutionContext.global

            implicit val ResultMonad = new Monad[ResultT] {
                override def flatMap[A, B](fa: ResultT[A])(f: A => ResultT[B]): ResultT[B] = fa flatMap { a => f(a) }

                override def tailRecM[A, B](a: A)(f: A => ResultT[Either[A, B]]): ResultT[B] = flatMap(f(a)) {
                    case Left(e) => tailRecM(e)(f)
                    case Right(b) => pure(b)
                }

                override def pure[A](x: A): ResultT[A] = EitherT {
                    WriterT {
                        Future {
                            (Vector.empty, x.asRight)
                        }
                    }
                }
            }

            /** 实现业务运算 */
            implicit object BusinessInterpreter extends Compiler[ResultT] {
                override def i2f(i: Int): ResultT[BigDecimal] = {
                    if (i >= 0) {
                        EitherT {
                            WriterT {
                                Future {
                                    (
                                            Vector(IO(logger.debug("i2f"))),
                                            BigDecimal(i).asRight // 告知顶层的 EitherT 将这个值转载如 right
                                    )
                                }
                            }
                        }
                    }
                    else {
                        EitherT {
                            WriterT {
                                Future {
                                    (
                                            Vector(IO(logger.debug("i2f"))),
                                            new RuntimeException("Input is smaller then 0").asLeft
                                    )
                                }
                            }
                        }
                    }
                }

                override def f2s(f: BigDecimal): ResultT[String] = {
                    EitherT {
                        WriterT {
                            Future {
                                (
                                        Vector(IO(logger.debug("f2s"))),
                                        f.toString.asRight
                                )
                            }
                        }
                    }
                }

                override def f2b(f: BigDecimal): ResultT[String] = {
                    EitherT {
                        WriterT {
                            Future {
                                (
                                        Vector(IO(logger.debug("f2b"))),
                                        (if (f < 1) false else true).toString.asRight
                                )
                            }
                        }
                    }
                }
            }
        }


        /*****************************************
          * 5) 使用
          * */
        object Client {
            import ADTs.ResultT
            import Implement._          // import 某个实现(隐式)

            def apply() = {
                val interpreter: Compiler[ResultT] = implicitly[Compiler[ResultT]]

                List(-1,0,1,2).foreach { i =>
                    val computation: ResultT[_] = interpreter(Alg.comp(i))   // Call DSL
                    Await.result(computation.value.run, Duration.Inf) match {
                        case (logs, res) => {
                            logs.foreach(_.unsafeRunSync())
                            res match {
                                case Left(e:Throwable) => println(e.getMessage)
                                case Right(a) => println(a)
                            }
                        }
                    }
                }
            }
        }

        Client()
    }
}
