package com.hibiup

import java.util.{Calendar, Date}

import scalaz.{-\/, Kleisli, NonEmptyList, \/, \/-}

/**
  * 演示一个具有外部依赖的实例
  * */

/**
  * 定义时
  */
package repository {
    /** Repository 是要被引入的外部模块 */
    trait AccountRepository {
        def query(no:String)
    }
}

object types {
    type Valid[A] = NonEmptyList[String] \/ A // 相当于 Either[NonEmptyList[String], A]

    type ValidedOperation[A, B] = Kleisli[Valid, A, B]

    type Amount = BigDecimal
}

package service {
    /**
      * 1) 显示引入依赖：只引入代数
      **/
    import repository._
    import types._

    // AccountService 模块
    trait AccountService[Account, Amount] {
        /** 2) 用 Kleisli 来组合注入的外部依赖！！ */
        type AccountRepositoryOperation= Kleisli[Valid, AccountRepository, Account]

        def open(no: String, name: String, rate: Option[BigDecimal], openingDate: Option[Date]): AccountRepositoryOperation
        def close(no: String, closeDate: Option[Date]): AccountRepositoryOperation
        def debit(no: String, amount: Amount): AccountRepositoryOperation
        def credit(no: String, amount: Amount): AccountRepositoryOperation

        /** 因为所有的函数返回都是基于 Kleisli　的，因此函数之间可以组合　 */
        def transfer(from: String, to: String, amount: Amount): AccountRepositoryOperation = for {
            a <- debit(from, amount)
            b <- credit(to, amount)
        } yield (a, b)
    }

    // InterestPostingService 模块
    trait InterestPostingService[Account, Amount] {
        import types._

        type InterestOperation = ValidedOperation[Account, Amount]
        type TaxOperation = ValidedOperation[Amount, Amount]

        def computeInterest: InterestOperation //Kleisli[Valid, Account, Amount]
        def computeTax: TaxOperation
    }
}


/**
  * 实现部分应尽可能推迟
  * */
package model {
    case class Account(no:String)
}

package repository.interpreter {
    import repository._

    private class AccountRepositoryInMemory extends AccountRepository {
        override def query(no: String): Unit = {
            ??? /* TODO: ... */
        }
    }

    object AccountRepository extends AccountRepositoryInMemory
}

package service.intercepter {
    import model._
    import service._
    import types._

    private class AccountServiceInterpreter extends AccountService[Account, Amount] {
        override def debit(no: String, amount: Amount): AccountRepositoryOperation =
            Kleisli {
                /** 返回 Kleisli !! */
                ??? /* TODO: ... */
            }

        override def credit(no: String, amount: Amount): AccountRepositoryOperation =
            Kleisli {
                /** 返回 Kleisli !! */
                ??? /* TODO: ... */
            }

        override def open(no: String, name: String, rate: Option[BigDecimal], openingDate: Option[Date]): AccountRepositoryOperation =
            Kleisli {
                /** 返回 Kleisli !! */
                ??? /* TODO: ... */
            }

        override def close(no: String, closeDate: Option[Date]): AccountRepositoryOperation =
            Kleisli {
                /** 返回 Kleisli !! */
                ??? /* TODO: ... */
            }
    }
    /** 实例化 AccountService 的伴随对象 */
    object AccountService extends AccountServiceInterpreter

    /** So do other modules ...*/
    private class InterestPostingServiceInterpreter extends InterestPostingService[Account, Amount] {
        override def computeInterest: InterestOperation = Kleisli {
            /** 返回 Kleisli !! */
            ??? /* TODO: ... */
        }

        override def computeTax: TaxOperation = Kleisli {
            /** 返回 Kleisli !! */
            ??? /* TODO: ... */
        }
    }
    object InterestPostingService extends InterestPostingServiceInterpreter
}

/** 用户端 */
object Example_14_Independence_Injection extends App{
    // 引入 ADT
    import model._
    import types._

    // 引入模块的实现
    import repository.interpreter._
    import service.intercepter.AccountService._
    import service.intercepter.InterestPostingService._

    /** 组合 AccountService 内的函数实现业务流程 */
    def postTransactions(a: Account, creditAmount: Amount, debitAmount: Amount): AccountRepositoryOperation =
        for {
            _ <- credit(a.no, creditAmount)
            d <- debit(a.no, debitAmount)
        } yield d

    /** 跨模块的功能组合 */
    import repository._
    type ComposeFinalOperation = ValidedOperation[AccountRepository, Amount]

    def composite(no: String, name: String, creditAmount: Amount, debitAmount: Amount): ComposeFinalOperation = (for {
        a <- open(no, name, Option(BigDecimal(0.4)), None)
        t <- postTransactions(a, creditAmount, debitAmount)
    } yield t) andThen computeInterest andThen computeTax

    /** Kick off 计算 */
    val x = composite("a-123", "John k", 10000, 2000)(AccountRepository)
}
