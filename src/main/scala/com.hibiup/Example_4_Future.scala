package com.hibiup

import java.util.Date

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


object Example_4_Future extends App{
    implicit val ec = global

    type Amount = BigDecimal
    final case class Balance(amount: Amount = 0)

    sealed trait Account{
        def no: Int
        def name: String
        def dateOfOpening: Date
        def balance: Balance
    }
    final case class CheckingAccount(no:Int, name: String, balance:Balance=Balance(0), dateOfOpening: Date=new Date()) extends Account

    trait InterestBearingAccount extends Account {
        def rateOfInterest: BigDecimal = 1
    }
    final case class SavingsAccount private(no:Int, name: String, balance:Balance=Balance(0), dateOfOpening: Date=new Date()) extends InterestBearingAccount
    final case class MoneyMarketAccount(no:Int, name: String, balance:Balance=Balance(0), dateOfOpening: Date=new Date()) extends InterestBearingAccount

    def calculateInterest[A <: SavingsAccount](account: A,
                                               balance: BigDecimal):Future[BigDecimal] = Future {
        println(s"Thread[${Thread.currentThread.getId}] - calculateInterest(): ${new Date()}")
        if (account.rateOfInterest == 0) throw new Exception("Interest Rate not found")
        else BigDecimal(10000)
    }

    def getCurrencyBalance[A <: SavingsAccount](account: A):Future[BigDecimal] = Future {
        println(s"Thread[${Thread.currentThread.getId}] - getCurrencyBalance(): ${new Date()}")
        BigDecimal(1000L)
    }

    def calculateNetAssetValue[A <: SavingsAccount](account: A,
                                                    ccyBalance: BigDecimal,
                                                    interest: BigDecimal): Future[BigDecimal] = Future {
            println(s"Thread[${Thread.currentThread.getId}] - calculateNetAssetValue(): ${new Date()}")
            ccyBalance + interest + 200
        }

    val a = SavingsAccount(1, "John")

    val result = for {
        b <- getCurrencyBalance(a)
        i <- calculateInterest(a, b)
        v <- calculateNetAssetValue(a, b, i)
    } yield (v)
    result onComplete {
        case Success(v) => println(s"Thread[${Thread.currentThread.getId}] - Success")
        case Failure(ex) => println(s"Thread[${Thread.currentThread.getId}] - Failure")
    }

    println(s"Thread[${Thread.currentThread.getId}] - main(): ${new Date()}")
    Await.result(result, 100 seconds)
    println(s"Thread[${Thread.currentThread.getId}] - main(): ${new Date()}")
}
