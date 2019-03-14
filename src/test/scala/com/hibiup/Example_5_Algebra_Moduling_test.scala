package com.hibiup

import java.util.Date

import org.scalacheck.Prop.forAll

import scala.util.{Success, Try}
import Example_5_Algebra_Moduling.ADTs._
import com.hibiup.Example_5_Algebra_Moduling.AccountService
import org.scalatest._


/** User: 实现合约中定义的代数方法　*/
case object AccountService extends AccountService[Account, Amount, Balance]() {
    override def open(no: String, name: String, openDate: Option[Date]): Try[Account] = ???
    override def close(account: Account, closeDate: Option[Date]): Try[Account] = ???
    override def debit(account: Account, amount: Amount): Try[Account] = ???
    override def credit(account: Account, amount: Amount): Try[Account] = ???
    override def balance(account: Account): Try[Balance] = ???
}

class Example_5_Algebra_Moduling_test extends PropSpec{
    override def withFixture(test: NoArgTest) = { // Define a shared fixture
        // Shared setup (run at beginning of each test)
        try test()
        finally {
            // Shared cleanup (run at end of each test)
        }
    }

    /** 支持引用透明 */
    /*property("Equal credit & debit in sequence retain the same balance") {
        import AccountService._
        forAll((a: Account, m: BigDecimal) => {
            val Success((before, after)) = for {
                b <- balance(a)
                c <- credit(a, m)
                d <- debit(c, m)
            } yield (b, d.balance)
            before == after
        })
    }*/
}
