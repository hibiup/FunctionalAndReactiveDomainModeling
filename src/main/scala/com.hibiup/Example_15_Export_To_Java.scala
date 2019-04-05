package com.hibiup.example15

import cats.data.{EitherNel, Kleisli, NonEmptyList}
import cats.implicits._

package Example_15_Export_To_Java {
    package object types {
        type Valid[A] = EitherNel[String, A]
        type ValidOperation[A, B] = Kleisli[Valid, A, B]
        type Amount = BigDecimal
    }

    trait InterestService[Account] {
        import types._

        type InterestOperation = ValidOperation[Account, Option[Amount]]
        type TaxOperation = ValidOperation[Option[Amount], Amount]

        /** 不能直接将 higher kind type 暴露给 Java．因为 Java 编译器会忽略内部类型
          *
          * https://stackoverflow.com/questions/55528032/how-to-export-scala-transformation-to-java
          * */
        def _computeInterest: InterestOperation = Kleisli {account =>
            computeInterest(account)
        }
        /** 因此要将关系将高级类型剥离后交给 Java 实现. */
        def computeInterest(acount:Account):Valid[Option[Amount]]

        /** 否则 Intellij 可能产生警告,但是依然可以编译：
          *
          * > sbt clean compile
          * */
        def computeTax: TaxOperation

        def op: ValidOperation[Account, Amount] = _computeInterest andThen computeTax
    }

    case class Account()

    object Client extends App {
        implicit val service = new JavaInterestService[Account]
        val s: JavaInterestService[Account] = implicitly
        s.op(Account())
    }
}
