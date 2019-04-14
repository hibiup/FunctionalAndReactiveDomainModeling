package com.hibiup

package Example_17_Free_Kleisli_State_Logger {
    import cats.data._
    import cats.effect.IO
    import cats.free.Free
    import cats.{Monad, ~>}
    import cats.implicits._
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory

    import scala.concurrent.duration.Duration
    import scala.concurrent.{Await, ExecutionContextExecutor, Future}

    object FreeWriterLogger extends App{
        object ADTs {
            /***************************************
              * 1) ADT
              * */
            trait Result[+A]
            final case class I2F(i:Int) extends Result[BigDecimal]
            final case class F2S(f:BigDecimal) extends Result[String]
            final case class F2B(f:BigDecimal) extends Result[String]

            /****************************************
              * 2) Lift
              * */
            type ResultA[A] = Free[Result, A]
        }


        /**************************************
          * 3) 定义 DSL (代数)
          * */
        object DSLs {
            import ADTs._

            final private def i2f(i:Int):ResultA[BigDecimal] = Free.liftF[Result, BigDecimal](I2F(i))
            final private def f2s(f:BigDecimal):ResultA[String] = Free.liftF[Result, String](F2S(f))
            final private def f2b(f:BigDecimal):ResultA[String] = Free.liftF[Result, String](F2B(f))

            /* Free 组合 */
            final def comp(i:Int):ResultA[String] = for {
                f <- i2f(i)
                s <- if (f<=1) f2b(f) else f2s(f)   /** 在 for-comprehension 里实现逻辑分支. */
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
            def apply[A](action: ResultA[A])(implicit monad:Monad[M]): M[A] = action.foldMap(route)

            private val route: Result ~> M = new (Result ~> M) {
                override def apply[A](fa: Result[A]): M[A] = fa match {
                    case I2F(i) => i2f(i).asInstanceOf[M[A]]
                    case F2S(f) => f2s(f).asInstanceOf[M[A]]
                    case F2B(f) => f2b(f).asInstanceOf[M[A]]
                }
            }

            protected def i2f(i:Int): M[BigDecimal]
            protected def f2s(f:BigDecimal): M[String]
            protected def f2b(f:BigDecimal): M[String]
        }


        /****************************************************
          * 业务实现
          * */
        object Implement {
            val logger = Logger(LoggerFactory.getLogger(this.getClass))
            implicit val ec: ExecutionContextExecutor =  scala.concurrent.ExecutionContext.global

            /**
             * 到实现的时候才定义返回值的容器类型
             */
            type Report = Vector[IO[Unit]]
            type λ[α] = WriterT[Future, Report, α]
            type ResultM[A] = EitherT[λ, Throwable, A]

            /*
             * 辅助 ResultT 运算的 Monad. Cats 和 Scalaz 提供了大部分基本的数据类型的 Monad，因此大部分情况下不需要自己实现．
             * */
            implicit val ResultMonad:Monad[ResultM] = new Monad[ResultM] {
                override def flatMap[A, B](fa: ResultM[A])(f: A => ResultM[B]): ResultM[B] = fa flatMap { a => f(a) }

                override def tailRecM[A, B](a: A)(f: A => ResultM[Either[A, B]]): ResultM[B] = flatMap(f(a)) {
                    case Left(e) => tailRecM(e)(f)
                    case Right(b) => pure(b)
                }

                override def pure[A](x: A): ResultM[A] = EitherT {
                    WriterT {
                        Future {
                            (Vector.empty, x.asRight)
                        }
                    }
                }
            }

            /**
              * 实现业务运算
              * */
            implicit object BusinessInterpreter extends Compiler[ResultM] {
                override protected def i2f(i: Int): ResultM[BigDecimal] = {
                    if (i >= 0) {
                        /**
                         * 根据实现时的需要，实现返回值的装箱.
                         * */
                        EitherT {
                            WriterT {
                                Future {
                                    // 返回值（可以拆分出纯业务函数去实现）
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

                override protected def f2s(f: BigDecimal): ResultM[String] = {
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

                override protected def f2b(f: BigDecimal): ResultM[String] = {
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
            import Implement._          // import 某个实现(隐式)

            def apply():Unit = {
                val interpreter: Compiler[ResultM] = implicitly[Compiler[ResultM]]

                List(-1,0,1,2).foreach { i =>
                    val computation: ResultM[_] = interpreter(DSLs.comp(i))   // Call DSL
                    Await.result(computation.value.run, Duration.Inf) match {
                        case (logs, res) =>
                            logs.foreach(_.unsafeRunSync())
                            res match {
                                case Left(e:Throwable) => println(e.getMessage)
                                case Right(a) => println(a)
                            }
                    }
                }
            }
        }

        Client()
    }
}
