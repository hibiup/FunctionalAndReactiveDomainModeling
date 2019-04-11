package com.hibiup

import com.typesafe.scalalogging.Logger
import org.scalatest.FlatSpec
import org.slf4j.LoggerFactory
import scalaz.{State, \/}
import scalaz.effect.IO
import scalaz._
import Scalaz._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


class Example_13_Kleisli_State_Logger extends FlatSpec{
    val logger = Logger(LoggerFactory.getLogger(this.getClass))

    "A State for logging" should "" in {
        implicit val ec =  scala.concurrent.ExecutionContext.global

        def logState[A](s:IO[Unit], a:A): State[List[IO[Unit]], A] = State[List[IO[Unit]], A]{ logs =>
            (logs :+ s, a)
        }

        type Valid[A] = Exception \/ A
        type Report = List[IO[Unit]]
        type StateResult[A] = State[Report, Valid[A]]

        def i2f: Kleisli[Future, Int, StateResult[BigDecimal]] = Kleisli{ i =>
            Future {
                State( logs =>(
                        logs :+ IO(logger.debug("i2f")),
                        if (i >= 0) BigDecimal(i).right else (new RuntimeException("Input is smaller then 0")).left)
                )
            }
        }

        def f2s(report:List[IO[Unit]]): Kleisli[Future, StateResult[BigDecimal], (Report, Valid[String])] = Kleisli { state =>
            Future {
                state(report :+ IO(logger.debug("f2s"))) match {
                        case (logs, \/-(f)) => (logs, f.toString.right)
                        case (logs, -\/(e)) => (logs, e.left)
                    }
            }
        }

        def comp(report:List[IO[Unit]]): Kleisli[Future, Int, (Report, Valid[String])] = i2f andThen f2s(report)

        val report:List[IO[Unit]] = List.empty

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
        implicit val ec =  scala.concurrent.ExecutionContext.global

        type Valid[A] = Exception \/ A
        type Report = List[IO[Unit]]
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

        Await.result(comp(-2)(List.empty), Duration.Inf) match {
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
        implicit val ec =  scala.concurrent.ExecutionContext.global

        type Valid[A] = Exception \/ A
        type Report = List[IO[Unit]]
        type StateResultT[A] = StateT[Future, Report, Valid[A]]

        implicit val StateResultBind: Bind[StateResultT] = new Bind[StateResultT] {
            override def bind[A, B](fa: StateResultT[A])(f: A => StateResultT[B]): StateResultT[B] = fa flatMap {
                case \/-(a: A) => f(a)
                case -\/(e) => fa map { case -\/(_) => e.left }
            }

            override def map[A, B](fa: StateResultT[A])(f: A => B): StateResultT[B] = fa map {
                case \/-(a:A) => f(a).right
                case -\/(e) => e.left
            }
        }

        def i2f: Kleisli[StateResultT, Int, BigDecimal] = Kleisli[StateResultT, Int, BigDecimal] { i =>
            StateT { logs:Report =>
                Future (
                    /** 问题：不知道什么原因会执行两遍 */
                    logs :+ IO(logger.debug("i2f")),
                    if (i >= 0) BigDecimal(i).right else (new RuntimeException("Input is smaller then 0")).left
                )
            }
        }

        def f2s: Kleisli[StateResultT, BigDecimal, String] = Kleisli[StateResultT, BigDecimal, String] { s =>
            StateT { logs =>
                Future (
                    logs :+ IO(logger.debug("f2s")),
                    s.toString.right
                )
            }
        }

        def comp: Kleisli[StateResultT, Int, String] = i2f andThen f2s

        Await.result(comp(-2)(List.empty), Duration.Inf) match {
            case (logs:Report, a) => {
                logs.foreach(_.unsafePerformIO())
                a match {
                    case \/-(s) => println(s"Finally we get: ${s}")
                    case -\/(e) => println(e.getMessage)
                }
            }
        }
    }
}
