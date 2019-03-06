package com.hibiup

import org.scalatest.FlatSpec

class Example_1_Trait_ADT_test extends FlatSpec{
    "AccountService in Example_1" should "" in {
        import com.hibiup.Example_1_Trait_ADT._

        val result = AccountService()
        println(result)

        assert(result.isSuccess)
        assert(result.get.balance == Balance(610))
    }

    "function compose" should "" in {
        val increase = (n: Int) => n + 1
        val square = (n: Int) => n * n

        println((1 to 10) map increase)
        println((1 to 10) map square)

        val incAndSquare = increase andThen square
        println((1 to 10) map incAndSquare)

        val squareAndInc = increase compose square
        println((1 to 10) map squareAndInc)
    }
}
