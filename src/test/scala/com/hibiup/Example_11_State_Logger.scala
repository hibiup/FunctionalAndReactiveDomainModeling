package com.hibiup

import com.typesafe.scalalogging.Logger
import org.scalatest.FlatSpec
import org.slf4j.LoggerFactory
import scalaz.State
import scalaz.effect.IO

class Example_11_State_Logger extends FlatSpec {
    val logger = Logger(LoggerFactory.getLogger(this.getClass))

    "A State for logging" should "" in {
        def logState[A](s:IO[Unit], a:A): State[List[IO[Unit]], A] = State[List[IO[Unit]], A]{ logs =>
            (logs :+ s, a)
        }

        def i2f(i:Int):BigDecimal = BigDecimal(i)
        def f2s(f:BigDecimal):String = f.toString()

        val logging: Int => State[List[IO[Unit]], String] = i => for{
            f <- logState(IO{ logger.info(s" => i2f($i)")}, i2f(i))
            s <- logState(IO{ logger.info(s" => f2s($f)")}, f2s(f))
        } yield s

        logging(2)(List.empty) match {
            case (logs, a) => {
                logs.foreach(_.unsafePerformIO())
                println(s"Finally we get $a")
            }
        }
    }
}
