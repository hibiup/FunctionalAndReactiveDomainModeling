package com.hibiup

import java.util.Date

/** *
  * https://github.com/debasishg/frdomain/blob/master/src/main/scala/frdomain/ch4/patterns/Applicative.scala
  *
  */
object Example_10_Applicative extends App {
    //trait Validation[E, A]
    type V[A] = Either[String, A]

    def validateAccountNo(no: String): V[String] = Right(no)
    def validateOpenCloseDate(openDate: Date, closeDate: Option[Date]): V[(Date, Option[Date])] = Right((openDate, closeDate))
    def validateRate(rate: BigDecimal): V[BigDecimal] = Right(rate)

    def apply3[V[_], A, B, C, D](va: V[A], vb: V[B], vc: V[C])(f: (A, B, C) => D): V[D] = ???
    def lift3[V[_], A, B, C, D](f: (A, B, C) => D): (V[A], V[B], V[C]) => V[D] = apply3(_, _, _)(f)


    sealed trait Currency
    case object USD extends Currency
    case object JPY extends Currency
    case object AUD extends Currency
    case object INR extends Currency

    case class Money(m: Map[Currency, BigDecimal]) {
        def toBaseCurrency: BigDecimal = ??? //..
    }

    case class Balance(b: Money)

    sealed trait Account {
        def no:String
        def name:String
        def openDate: Date
        def balance: Balance
    }

    final case class SavingsAccount(no: String,
                                    name: String,
                                    balance: Balance,
                                    openDate: Date,
                                    closeDate: Option[Date],
                                    rate:BigDecimal) extends Account

    def savingsAccount(no: String, name: String, rate: BigDecimal,
                       openDate: Date, closeDate: Option[Date],
                       balance: Balance): V[Account] = {
        apply3(
            validateAccountNo(no),
            validateOpenCloseDate(openDate, closeDate),
            validateRate(rate)
        ) { (n, d, r) =>
            SavingsAccount(n, name, balance, d._1, d._2, r)
        }
    }

    //savingsAccount("1", "John", BigDecimal(0.2), new Date, Option(null), Balance(Money(Map{USD -> BigDecimal(100)})))


    /************************************/
    trait Functor[F[_]] {
        def map[A, B](a: F[A])(f: A => B): F[B]
    }

    trait Applicative[F[_]] extends Functor[F] {
        def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
        def apply2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(f.curried))
        def unit[A](a: A): F[A]
    }

    val a = new Applicative[List](){
        def unit[A](a: A): List[A] = List(a)
        override def ap[A, B](fa: List[A])(f: List[A => B]): List[B] = for {
            a <- fa
            f <- f
        } yield f(a)
        override def map[A, B](a: List[A])(f: A => B): List[B] = a map f
    }

    println(a.ap(List(1,2,3))(List((x: Int) => x * x)))
    println(a.apply2(List(1,2,3), List(4,5,6))((x: Int, y:Int) => x + y))



    /** 十年前的定义 **************************************/
    object DesignFor10YearsAgo {
        // 面向合约
        def func(a:Int)(implicit f:Int => Unit) = f(a)
    }

    // 十年前的实现
    import DesignFor10YearsAgo._
    // 提供实现
    implicit def old_func(a: Int): Unit = println(a)
    func(2)    // 2

    /** 十年后再设计 ******************************************/
    object ReDesignFor10YearsAfter {
        // 面向合约的 curry
        implicit def f_curry(implicit f: (Int, Int) => Unit) = f(_, 2)
    }

    // 十年后的再实现
    import ReDesignFor10YearsAfter._
    // 提供新的实现
    implicit def new_func(a:Int, b:Int): Unit = println(a,b)
    func(2)    // (2,2)
}
