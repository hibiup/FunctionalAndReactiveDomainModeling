package com.hibiup

import org.scalatest.FlatSpec

import scala.util.Success

class Example_2_Compose_test extends FlatSpec {
    "AccountService in Example_2" should "" in {
        import com.hibiup.Example_2_Compose._

        val result = AccountService.interest()
        result.foreach{
            case Success(interest) => println(interest)
        }

        assert( AccountService.totalInterest() === BigDecimal(15.2) )

        val markedAccounts = AccountService.markAccount(1)
        assert(markedAccounts.length === 4)
        assert(markedAccounts.count(_.isSuccess) === 1)

        val x = AccountService.getAccountFrom(3)
        println(x)
        assert(x.isSuccess)
        x.map(a => assert(a.no === 3))

        val amount = x.flatMap(AccountService.getCurrencyBalance)
        println(amount)
        assert(amount == Success(0.75))

        val ass = AccountService.calculateNetAssetValue(2, 100)
        println(ass)
    }

    "andThen and compose" should "" in {
        val l = List(1,2,3,4)
        def twice: Int => Int = n => n + n
        def reduce:Int => Int = n => n-1
        println(l map twice map reduce)
        println(l map (twice andThen reduce))
        println(l map (twice compose reduce))
    }
}
