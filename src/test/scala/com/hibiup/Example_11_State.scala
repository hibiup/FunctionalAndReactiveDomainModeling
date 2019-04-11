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


class Example_11_State extends FlatSpec{
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
}
