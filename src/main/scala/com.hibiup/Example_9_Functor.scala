package com.hibiup

import java.util.{Calendar, Date}

import scala.util.{Failure, Success, Try}

object Example_9_Functor {
    sealed trait Currency
    case object USD extends Currency
    case object JPY extends Currency
    case object AUD extends Currency
    case object INR extends Currency

    case class Money(m: Map[Currency, BigDecimal]) {
        def toBaseCurrency: BigDecimal = ??? //..
    }

    case class Balance(b: Money)

    sealed trait Account {
        def no:String
        def name:String
        def openDate: Option[Date]
        def balance: Money
    }

    final case class SavingsAccount(no: String, name: String,
                                    openDate: Option[Date],
                                    balance: Money, rate:BigDecimal) extends Account

    /******************************************/
    def closeDateCheck(open: Date) = if (Calendar.getInstance.getTime.before(open)) Failure(new RuntimeException) else Success(open)

    def savingsAccount(no: String, name: String, rate: BigDecimal,
                       openDate: Date,
                       balance: Money): Try[Account] = {
        closeDateCheck(openDate).map { d =>
            if (rate <= BigDecimal(0))
                throw new Exception(s"Interest rate $rate must be > 0")
            else
                SavingsAccount(no, name, Option(d), balance, rate)
        }
    }
}
