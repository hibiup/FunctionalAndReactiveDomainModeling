package com.hibiup

import java.util.Date

import scala.util.Try

object Example_5_Algebra_Moduling {
    object ADTs {
        type Amount = BigDecimal
        final case class Balance(amount: Amount = 0)

        sealed trait Account{
            def no: Int
            def name: String
            def dateOfOpening: Date
            def balance: Balance
        }
        final case class SavingsAccount(no:Int, name: String, balance:Balance=Balance(0), dateOfOpening: Date=new Date()) extends Account

        final case class MoneyMarketAccount(no:Int, name: String, balance:Balance=Balance(0), dateOfOpening: Date=new Date()) extends Account
    }

    /**
      * 面向代数编程我们可以做到:
      **/

    /**　1)参数泛化，可以写成 [A, B, C] */
    trait AccountService[Account, Amount, Balance] {
        def open(no: String, name: String, openDate: Option[Date]): Try[Account]
        def close(account: Account, closeDate: Option[Date]): Try[Account]
        def debit(account: Account, amount: Amount): Try[Account]
        def credit(account: Account, amount: Amount): Try[Account]
        def balance(account: Account): Try[Balance]

        /** 2) 在没有实现 debit　和 credit 之前，就可以实现代数组合　*/
        def transfer(from: Account, to: Account, amount: Amount): Try[(Account, Account, Amount)] = for {
            a <- debit(from, amount)
            b <- credit(to, amount)
        } yield (a, b, amount)
    }
}
