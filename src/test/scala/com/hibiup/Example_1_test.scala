package com.hibiup

import org.scalatest.FlatSpec

class Example_1_test extends FlatSpec{
    "AccountService in Example_1" should "" in {
        import com.hibiup.Example_1._

        val result = AccountService()
        println(result)

        assert(result.isSuccess)
        assert(result.get.balance == Balance(610))
    }
}
