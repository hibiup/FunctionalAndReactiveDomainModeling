package com.hibiup.example_16

import java.util.{Calendar, Date}

import scala.util.{Success, Try}

object Example_16_Implicit_Type_Class extends App{
    /** 设计 **********************************************/
    /** 定义一个名为 Show 的 type class */
    trait Show[T] {
        def shows(t: T): Try[String]
    }

    /** 不同 Show 的类型类 */
    trait ShowProtocol[Account, Customer] {
        implicit val showAccount: Show[Account]
        implicit val showCustomer: Show[Customer]
    }

    /** 使用 Show 的场所 */
    object Reporting {
        /** 利用 context-bound 为 report 实现（ad hoc）多态。context-bound 是基于方法签名的，因此可以推迟实现。
          *
          * 特设多态又称为参数多态，可以用于类或方法上。
          * */
        def report[T: Show](as: Seq[T]) =
            as.map(implicitly[Show[T]].shows(_))   // 获得 context-bound 定义的隐式（取决于执行的时候提供了哪个实例）
    }

    /**　实现　**********************************************/
    type Balance = BigDecimal
    type Address = String
    case class Account(no: String, name: String,
                       dateOfOpening: Date = Calendar.getInstance.getTime,
                       dateOfClosing: Option[Date] = None,
                       balance: Balance = BigDecimal(0))
    case class Customer(no: String, name: String, address: Address, email: String)

    /** 实现不同的 Show，并配合 context-bound 将它们设置为隐式 */
    trait DomainShowProtocol extends ShowProtocol[Account, Customer] {
        implicit val showAccount: Show[Account] = (a: Account) => Success(a.toString)  // 实现 shows 方法
        implicit val showCustomer: Show[Customer] = (c: Customer) => Success(c.toString)
    }
    object DomainShowProtocol extends DomainShowProtocol

    /**　使用　**********************************************/
    /** 加载某个特定的隐式 */
    import DomainShowProtocol.showCustomer

    /** context-bound 会绑定到这个实例上，以实现多态。 */
    Reporting.report(Seq(
        Customer("1", "John", "Somewhere", "aa@abc.com")  // T = Customer
    ))
}
