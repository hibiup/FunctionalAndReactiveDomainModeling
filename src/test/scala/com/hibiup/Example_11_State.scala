package com.hibiup

import com.typesafe.scalalogging.Logger
import org.scalatest.FlatSpec
import org.slf4j.LoggerFactory
import scalaz.{State, \/}
import scalaz.effect.IO
import scalaz._
import Scalaz._


class Example_11_State extends FlatSpec{
    val logger = Logger(LoggerFactory.getLogger(this.getClass))

    "A State for logging" should "" in {
        def logState[A](s:IO[Unit], a:A): State[List[IO[Unit]], A] = State[List[IO[Unit]], A]{ logs =>
            (logs :+ s, a)
        }

        type Valid[A] = Exception \/ A

        def i2f(i:Int): Valid[BigDecimal] = if (i >= 0) BigDecimal(i).right else (new RuntimeException("Input is smaller then 0")).left
        def f2s(f: Valid[BigDecimal]): Valid[String] = f match {
            case \/-(f1) => f1.toString.right
            case -\/(e) => e.left
        }

        val comp: Int => State[List[IO[Unit]], Valid[String]] = i => for{
            f <- logState(IO{ logger.info(s" => i2f($i)")}, i2f(i))
            s <- logState(IO{ logger.info(s" => f2s($f)")}, f2s(f))
        } yield s

        comp(2)(List.empty) match {
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
