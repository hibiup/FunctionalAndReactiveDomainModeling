package com.hibiup

import java.util.Date

import scala.util.{Failure, Success, Try}

object Example_1_Trait_ADT extends App {
    // 各种 ADT
    type Amount = BigDecimal
    case class Balance(amount: Amount = 0)
    case class Account(no: String, name: String, dateOfOpening: Date, balance: Balance = Balance())

    // 行为
    trait AccountService {
        def debit(a: Account, amount: Amount): Try[Account] = {
            if (a.balance.amount < amount) Failure(new Exception("Insufficient balance in account"))
            else Success(a.copy(balance = Balance(a.balance.amount - amount))) // case class 对深拷贝并不友好．
        }
        def credit(a: Account, amount: Amount): Try[Account] =
            Success(a.copy(balance = Balance(a.balance.amount + amount)))
    }

    // 组合
    object AccountService extends AccountService {
        def apply() = {
            val a = Account("a1", "John", new Date)
            assert(a.balance == Balance(0))
            for {
                b <- credit(a, 1000)
                c <- debit(b, 200)
                d <- debit(c, 190)
            } yield d
        }
    }
}
