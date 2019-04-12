package com.hibiup

import com.typesafe.scalalogging.Logger
import org.scalatest.FlatSpec
import org.slf4j.LoggerFactory

class Example_13_Kleisli_State_Logger extends FlatSpec{
    val logger = Logger(LoggerFactory.getLogger(this.getClass))

    "A State for logging" should "" in {
        /**
          * State 可以用于日志处理, 它输入一个状态(List[IO[Unit]])用于日志的存储,返回存有日志的新状态, 和要执行的目标函数
          * */
        import scalaz.{State, \/}
        import scalaz.effect.IO
        import scalaz._
        import Scalaz._
        import scala.concurrent.duration.Duration
        import scala.concurrent.{Await, Future}

        implicit val ec =  scala.concurrent.ExecutionContext.global

        def logState[A](s:IO[Unit], a:A): State[Vector[IO[Unit]], A] = State[Vector[IO[Unit]], A]{ logs =>
            (logs :+ s, a)
        }

        type Valid[A] = Exception \/ A
        type Report = Vector[IO[Unit]]
        type StateResult[A] = State[Report, Valid[A]]

        def i2f: Kleisli[Future, Int, StateResult[BigDecimal]] = Kleisli{ i =>
            Future {
                State( logs =>(
                        logs :+ IO(logger.debug("i2f")),
                        if (i >= 0) BigDecimal(i).right else (new RuntimeException("Input is smaller then 0")).left)
                )
            }
        }

        def f2s(report:Vector[IO[Unit]]): Kleisli[Future, StateResult[BigDecimal], (Report, Valid[String])] = Kleisli { state =>
            Future {
                state(report :+ IO(logger.debug("f2s"))) match {
                        case (logs, \/-(f)) => (logs, f.toString.right)
                        case (logs, -\/(e)) => (logs, e.left)
                    }
            }
        }

        def comp(report:Vector[IO[Unit]]): Kleisli[Future, Int, (Report, Valid[String])] = i2f andThen f2s(report)

        val report:Vector[IO[Unit]] = Vector.empty

        Await.result(comp(report :+ IO{logger.debug("main")})(-2), Duration.Inf) match {
            case (logs, a) => {
                logs.foreach(_.unsafePerformIO())
                a match {
                    case \/-(s) => println(s"Finally we get: ${s}")
                    case -\/(e) => println(e.getMessage)
                }
            }
        }
    }

    "A StateT for logging (Version: 1)" should "" in {
        /**
          * 上例中如果存在多重分装，例如 Future[State[...]]，那么就需要 StateT 将返回值提取出来
          * */
        import scalaz.{State, \/}
        import scalaz.effect.IO
        import scalaz._
        import Scalaz._
        import scala.concurrent.duration.Duration
        import scala.concurrent.{Await, Future}

        implicit val ec =  scala.concurrent.ExecutionContext.global

        type Valid[A] = Exception \/ A
        type Report = Vector[IO[Unit]]
        type StateResultT[A] = StateT[Future, Report, A]

        implicit val StateResultBind: Bind[StateResultT] = new Bind[StateResultT] {
            override def bind[A, B](fa: StateResultT[A])(f: A => StateResultT[B]): StateResultT[B] = fa flatMap f
            override def map[A, B](fa: StateResultT[A])(f: A => B): StateResultT[B] = fa map f
        }

        def i2f: Kleisli[StateResultT, Int, Valid[BigDecimal]] = Kleisli{ i =>
            StateT { logs =>
                Future (
                    logs :+ IO(logger.debug("i2f")),
                    if (i >= 0) BigDecimal(i).right else (new RuntimeException("Input is smaller then 0")).left
                )
            }
        }

        /**
          * 需要改进:
          *     i2f 失败的话, 就不应该继续执行 f2s,但是因为传入参数是 Valid[_] 总是有效, 因此 f2s 总是会被执行到. 这需要修改.
          * */
        def f2s: Kleisli[StateResultT, Valid[BigDecimal], Valid[String]] = Kleisli { state =>
            StateT { logs =>
                Future (
                    logs :+ IO(logger.debug("f2s")),
                    state match{
                        case \/-(f) => f.toString.right
                        case -\/(e) => e.left
                    }
                )
            }
        }

        def comp: Kleisli[StateResultT, Int, Valid[String]] = i2f andThen f2s

        Await.result(comp(-2)(Vector.empty), Duration.Inf) match {
            case (logs, a) => {
                logs.foreach(_.unsafePerformIO())
                a match {
                    case \/-(s) => println(s"Finally we get: ${s}")
                    case -\/(e) => println(e.getMessage)
                }
            }
        }
    }

    "A StateT for logging (Version: 2)" should "" in {
        /**
          * 修复上例中 StateResultT[A] 不能返回最终值得问题。
          * */
        import scalaz._
        import Scalaz._
        import scalaz.effect.IO
        import scalaz.concurrent.Task

        type Valid[A] = Exception \/ A
        type Report = Vector[IO[Unit]]
        type StateResultT[A] = StateT[Task, Report, Valid[A]]

        /**
          * 因为 StateResultT 是定制类型，需要自己实现一个 Bind(Monad) 来实现 flatMap(Bind) 和 map
          * */
        implicit val StateResultBind: Bind[StateResultT] = new Bind[StateResultT] {
            override def bind[A, B](fa: StateResultT[A])(f: A => StateResultT[B]): StateResultT[B] = fa flatMap {
                case \/-(a) => f(a)
                case -\/(e) => fa map { case -\/(_) => e.left }
            }

            override def map[A, B](fa: StateResultT[A])(f: A => B): StateResultT[B] = fa map {
                case \/-(a) => f(a).right
                case -\/(e) => e.left
            }
        }

        def i2f: Kleisli[StateResultT, Int, BigDecimal] = Kleisli[StateResultT, Int, BigDecimal] { i =>
            StateT { logs:Report =>
                Task (
                    /** 问题：不知道什么原因会执行两遍 */
                    logs :+ IO(logger.debug(s"[Thread-${Thread.currentThread.getId}] - i2f")),
                    if (i >= 0) BigDecimal(i).right else (new RuntimeException("Input is smaller then 0")).left
                )
            }
        }

        def f2s: Kleisli[StateResultT, BigDecimal, String] = Kleisli[StateResultT, BigDecimal, String] { s =>
            StateT { logs =>
                Task (
                    logs :+ IO(logger.debug(s"[Thread-${Thread.currentThread.getId}] - f2s")),
                    s.toString.right
                )
            }
        }

        def comp: Kleisli[StateResultT, Int, String] = i2f andThen f2s

        comp(2)(Vector.empty).flatMap { x => Task { x match {
            case (logs:Report, a) => {
                logs.foreach(_.unsafePerformIO())
                a match {
                    case \/-(s) => println(s"[Thread-${Thread.currentThread.getId}] - Finally we get: ${s}")
                    case -\/(e) => println(s"[Thread-${Thread.currentThread.getId}] - ${e.getMessage}")
                }
            }
        }}}.unsafePerformSync
    }

    /**
      * Writer 和 State 类似, 是一个为日志这类应用定制了的 State，它不需要传入参数(logs), 直接通过 tell 方法接受日志和要执行的
      * 目标函数
      *
      * http://eed3si9n.com/herding-cats/Writer.html
      *
      * */
    "A Writer for logging" should "" in {
        import cats._
        import cats.data._
        import cats.implicits._
        import cats.effect.IO

        /** 函数返回 Writer (和上例返回 State 类是) */
        def gcd(a: Int, b: Int): Writer[Vector[IO[Unit]], Int] = {
            // 添加 n 条日志
            if (b == 0) for {
                    _ <- Writer.tell(Vector(IO(logger.info("Finished with " + a.show))))
                } yield a
            else
                Writer.tell(Vector(IO(logger.info(s"${a.show} mod ${b.show} = ${(a % b).show}")))) >>= { _ =>
                    gcd(b, a % b)
                }
        }

        // 不需传入 logs
        val (logs, result) = gcd(12, 16).run  // s:Writer
        logs.foreach(l => l.unsafeRunSync())
        println(s"Finally we get: ${result}")
    }


    "A WriterT for logging" should "" in {
        import cats._
        import cats.data._
        import cats.implicits._
        import cats.effect.IO
        import scala.concurrent.duration.Duration
        import scala.concurrent.{Await, Future}

        implicit val ec =  scala.concurrent.ExecutionContext.global

        type Valid[A] = Either[Throwable, A]
        type Report = Vector[IO[Unit]]
        type StateResultT[A] = WriterT[Future, Report, Valid[A]]

        /**
          * 因为 StateResultT 是定制类型，需要自己实现一个 Bind(Monad) 来实现 flatMap(Bind) 和 map
          * */
        implicit val StateResultBind: FlatMap[StateResultT] = new FlatMap[StateResultT] {
            override def tailRecM[A, B](a: A)(f: A => StateResultT[Either[A, B]]): StateResultT[B] = ???

            override def flatMap[A, B](fa: StateResultT[A])(f: A => StateResultT[B]): StateResultT[B] = fa flatMap {
                case Right(a) => f(a)
                case Left(e) => fa map { case Left(_) => e.asLeft }
            }

            override def map[A, B](fa: StateResultT[A])(f: A => B): StateResultT[B] = fa map {
                case Right(a) => f(a).asRight
                case Left(e) => e.asLeft
            }
        }

        def i2f: Kleisli[StateResultT, Int, BigDecimal] = Kleisli[StateResultT, Int, BigDecimal] { i =>
            WriterT {
                Future (
                    (Vector(IO(logger.debug("i2f"))),
                    if (i >= 0) BigDecimal(i).asRight  else new RuntimeException("Input is smaller then 0").asLeft)
                )
            }
        }

        def f2s: Kleisli[StateResultT, BigDecimal, String] = Kleisli[StateResultT, BigDecimal, String] { f =>
            WriterT {
                Future ((
                        Vector(IO(logger.debug("f2s"))), f.toString.asRight
                ))
            }
        }

        def comp: Kleisli[StateResultT, Int, String] = i2f andThen f2s

        Await.result(comp(-2).run, Duration.Inf) match {
            case (logs:Report, a) => {
                logs.foreach(_.unsafeRunSync())
                a match {
                    case Right(s) => println(s"Finally we get: $s")
                    case Left(e) => println(e.getMessage)
                }
            }
        }
    }


    "EitherT and WriterT works together for 'for-comprehension' " should "" in {
        import cats._
        import cats.data._
        import cats.implicits._
        import cats.effect.IO
        import scala.concurrent.duration.Duration
        import scala.concurrent.{Await, Future}

        implicit val ec =  scala.concurrent.ExecutionContext.global

        type Report = Vector[IO[Unit]]
        /**
          * 当 EitherT 和 WriterT 结合在一起的时候，一个恰当的定义貌似：
          *
          *     EitherT[WriterT[Future, Report, _], Throwable, A]
          *
          * 但是这个类型存在一个问题：Transformer 的第一个参数是一个容器，它的值是后两个参数定义的，并且不需要显示表达，
          * 但是因为 WriterT 接受三个参数，所以我们不得不明确告诉便一起请使用第三个参数作为预留位置。为此需要定义一个 type lambda 来明确指定位置:
          *
          *    type ValidT[A] = EitherT[({type λ[α] = WriterT[Future, Report,α]})#λ, Throwable, A]
          *
          * 这个表达式：定义一个 λ[α] 类型，它的参数用 α 表示，它的值等于：WriterT[Future, Report, α]。也就是说，我们名确定义了
          * WriterT 的第三个参数等于 λ 的参数，这样我们就得到了一个只接受一个参数的新的类型： λ。 更易读的形式可以写成两行：
          * */
        type λ[α] = WriterT[Future, Report, α]       // 定义 type lambda
        type ValidT[A] = EitherT[λ, Throwable, A]   // 应用　type lambda

        def i2fW(i:Int): ValidT[BigDecimal] = {
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

        def f2sW(f:BigDecimal): ValidT[String] = {
            EitherT {
                WriterT {
                    Future { (
                            Vector(IO(logger.debug("f2s"))),
                            f.toString.asRight
                    ) }
                }
            }
        }

        /** for-comprehension ************************************/
        def compFor(i:Int) = for {
            f <- i2fW(i)
            s <- f2sW(f)
        } yield s

        Await.result(compFor(-2).value.run, Duration.Inf) match {
            case (logs:Report, a) => {
                logs.foreach(_.unsafeRunSync())
                a match {
                    case Right(s) => println(s"Finally we get: $s")
                    case Left(e) => println(e.getMessage)
                }
            }
        }
    }
}
