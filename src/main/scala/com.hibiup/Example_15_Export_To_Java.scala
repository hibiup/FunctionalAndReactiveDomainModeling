package com.hibiup.example_15

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

        /**
          * １）不建议直接将 higher kind type 暴露给 Java．因为 Java 编译器会忽略内部类型
          *
          * https://stackoverflow.com/questions/55528032/how-to-export-scala-transformation-to-java
          * */
        def computeInterestK: InterestOperation = Kleisli {account =>
            computeInterest(account)
        }
        /** 将高级类型 Kleisli 剥离后交给 Java 去实现. */
        def computeInterest(acount:Account):Valid[Option[Amount]]

        /**
          * ２）如果直接暴露，需要在 Java 代码中移除 Either 的内部类型，并且增加以下配置到 build.sbt
          *
          * javacOptions ++= Seq(
          *                 "-Xlint:unchecked"
          *             )
          *
          * 然后执行：
          *
          * > sbt clean compile
          * */
        def computeTaxK: TaxOperation

        def op: ValidOperation[Account, Amount] = computeInterestK andThen computeTaxK
    }

    case class Account()

    object Client extends App {
        implicit val service = new JavaInterestService[Account]
        val s: JavaInterestService[Account] = implicitly
        s.op(Account())
    }
}
