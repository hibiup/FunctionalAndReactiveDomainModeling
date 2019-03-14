/** Example p50 (75/322) */

package com.hibiup

import java.util.Date

import scala.util.{Failure, Success, Try}

object Example_2_Compose {
    /** 各种 ADT */
    type Amount = BigDecimal
    final case class Balance(amount: Amount = 0)

    // 各种 Account
    sealed trait Account{
        def no: Int
        def name: String
        def dateOfOpening: Date
        def balance: Balance
    }
    final case class CheckingAccount(no:Int, name: String, balance:Balance=Balance(0), dateOfOpening: Date=new Date()) extends Account

    trait InterestBearingAccount extends Account {
        def rateOfInterest: BigDecimal = 0
    }
    final case class SavingsAccount(no:Int, name: String, balance:Balance=Balance(0), dateOfOpening: Date=new Date()) extends InterestBearingAccount
    final case class MoneyMarketAccount(no:Int, name: String, balance:Balance=Balance(0), dateOfOpening: Date=new Date()) extends InterestBearingAccount


    /** 行为 */
    trait AccountService {
        def calculateInterest[A <: InterestBearingAccount]: (A, Int) => Try[BigDecimal] = // “Try”: in case 会失败
            (account, period) => Success(account.balance.amount * period)
    }


    /** 组合 */
    object AccountService extends AccountService {
        val clients = List(
            CheckingAccount(1, "sb001", Balance(0.5)),
            SavingsAccount(2, "sb002", Balance(0.5)),
            SavingsAccount(3, "sb003", Balance(0.75)),
            SavingsAccount(4, "sb004", Balance(0.27))
        )

        def interest() =
            clients.filter(_.isInstanceOf[InterestBearingAccount]).asInstanceOf[List[InterestBearingAccount]]
                    .map(calculateInterest(_, 10))


        def totalInterest() =
            interest()
                    // “filter”： 只取成功的
                    .filter(_.isSuccess)
                    .foldLeft(BigDecimal(0))((t, c) =>
                        // "map": 从 Try 中取值
                        c.map(_ + t).getOrElse(t)    // "getOrElse": 如果得到 Failure （实际上 filtre 已经保障了）

            )

        def deductTax: BigDecimal => BigDecimal = { interest =>
            if (interest < 1000) interest else (interest - 0.1 * interest)
        }


        def markAccount(id:Int): List[Try[Account]] =
            clients.map {
                case a if a.no == id => Success(a)
                case a => Failure(new RuntimeException(s"Account ${a.no} is not the one we are interested"))
            }


        def getAccountFrom(no: Int): Try[Account] = Try(clients.filter(_.no == no).head)
        def getCurrencyBalance(a: Account): Try[Amount] = Success(a.balance.amount)
        def calculateNetAssetValue(no:Int, period:Int ): Try[(Amount, BigDecimal)] =
            for{
                a <- getAccountFrom(no)
                m <- getCurrencyBalance(a)
                i <- a match {
                    case x:SavingsAccount => calculateInterest(x, period)
                    case _ => Try(BigDecimal(0))
                }
                if (m + i) > 0
            } yield (m , i)
    }
}
