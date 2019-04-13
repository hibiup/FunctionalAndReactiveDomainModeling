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
        val logger = Logger(LoggerFactory.getLogger(this.getClass))
        implicit val ec =  scala.concurrent.ExecutionContext.global

        trait ADTs {
            /** 1) */
            trait Result[+A]
            case class I2F(i:Int) extends Result[BigDecimal]
            case class F2S(f:BigDecimal) extends Result[String]

            /** 2) */
            type ResultF[A] = Free[Result, A]
        }

        object ADTs extends ADTs {
            type Report = Vector[IO[Unit]]
            type λ[α] = WriterT[Future, Report, α]
            type ResultT[A] = EitherT[λ, Throwable, A]
        }

        trait ExpressionAlgs {
            import ADTs._
            /** 3) */
            def i2f(i:Int):ResultF[BigDecimal] = Free.liftF[Result, BigDecimal](I2F(i))
            def f2s(f:BigDecimal):ResultF[String] = Free.liftF[Result, String](F2S(f))

            /** Free 组合 */
            def i2s(i:Int):ResultF[String] = for {
                f <- i2f(i)
                s <- f2s(f)
            } yield s
        }

        import ADTs._
        trait ExpressionInterpreter[M[_]] {
            // 将一个 Free Monad 映射到一个带有业务运算的 Task Monad
            def apply[A](action: ResultF[A]): M[A]
        }

        implicit val StateResultMonad = new Monad[ResultT] {
            override def flatMap[A, B](fa: ResultT[A])(f: A => ResultT[B]): ResultT[B] = fa flatMap {a => f(a) }

            override def tailRecM[A, B](a: A)(f: A => ResultT[Either[A, B]]): ResultT[B] = flatMap(f(a)) {
                case Left(e) => tailRecM(e)(f)
                case Right(b) => pure(b)
            }

            override def pure[A](x: A): ResultT[A] = EitherT {
                WriterT {
                    Future {( Vector.empty, x.asRight )}
                }
            }
        }

        implicit object MyExpressionInterpreter extends ExpressionInterpreter[ResultT] {
            override def apply[A](action: ResultF[A]): ResultT[A] = action.foldMap(route)

            private val route: Result ~> ResultT = new (Result ~> ResultT) {
                override def apply[A](fa: Result[A]): ResultT[A] = fa match {
                    case I2F(i) => i2f(i).asInstanceOf[ResultT[A]]
                    case F2S(f) => f2s(f).asInstanceOf[ResultT[A]]
                }
            }

            def i2f(i:Int): ResultT[BigDecimal] = {
                if (i >= 0) {
                    EitherT {
                        WriterT {
                            Future { (
                                    Vector(IO(logger.debug("i2f"))),
                                    BigDecimal(i).asRight      // 告知顶层的 EitherT 将这个值转载如 right
                            ) }
                        }
                    }
                }
                else {
                    EitherT {
                        WriterT {
                            Future { (
                                    Vector(IO(logger.debug("i2f"))),
                                    new RuntimeException("Input is smaller then 0").asLeft
                            ) }
                        }
                    }
                }
            }

            def f2s(f:BigDecimal): ResultT[String] = {
                EitherT {
                    WriterT {
                        Future { (
                                Vector(IO(logger.debug("f2s"))),
                                f.toString.asRight
                        ) }
                    }
                }
            }
        }

        object ExpressionAlgs extends ExpressionAlgs
        import ExpressionAlgs._

        val _i2s: ResultT[_] = MyExpressionInterpreter(i2s(2))
        Await.result(_i2s.value.run, Duration.Inf) match {
            case (logs, a) => {
                logs.foreach(_.unsafeRunSync())
                a.foreach(println)
            }
        }
    }
}
