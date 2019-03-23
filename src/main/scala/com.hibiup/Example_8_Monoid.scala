package com.hibiup

import java.util.Date

object Example_8_Monoid {
    /*************************************************/
    /** 基本 ADT 数据类型
      *
      * Balance > Money > (Currency, BigDecimal)
      * */
    sealed trait Currency
    case object USD extends Currency
    case object JPY extends Currency
    case object AUD extends Currency
    case object INR extends Currency

    case class Money(m: Map[Currency, BigDecimal]) {
        def toBaseCurrency: BigDecimal = ??? //..
    }

    case class Balance(b: Money)

    /** 统计模块接口 */
    trait Analytics[Balance, Money] {
        /** 输入一个 Balance 列表, 返回数据类型为 Money 的总额 */
        def sumBalances(bs: List[Balance])(implicit m: Monoid[Money]): Money
    }

    /*************************************************/
    /** 为了剥离对 Balance 的运算让 sumBalances 更具通用性，定义一个 Monoid[T] 数据类型来隔离 sumBalances 和 BigDecimal。 */
    trait Monoid[T] {
        def zero: T
        def op(t1: T, t2: T): T
    }

    /** 为 BigDecimal 实现 Monoid，它的 op 方法实现了两个 BigDecimal 的累加运算。*/
    implicit def BigDecimalAdditionMonoid = new Monoid[BigDecimal] {
        val zero = BigDecimal(0)                                 // 定义 BigDecimal 的零值.
        def op(i: BigDecimal, j: BigDecimal) = i + j   // 为 BigDecimal 累加实现运算.
    }

    /** 因为不同的计算目的的 op 很可能不同，因此为不同的计算目的, 要实现不同的 Monoid 实例。
      *
      * 这个 Monoid 的 op 方法实现了两个 Money（Map）的相加运算．
      *
      * 因为 Money 用 Map 来存放（Currency, BigDecimal），最终实现的是两个 BigDecimal 的累加运算，
      * 同理为了剥离 BigDecimal 的累加运算，我们也需要为 BigDecimal 定义它的 Monoid．
      * */
    implicit def MoneyMonoid = new Monoid[Money] {
        def zero = Money(Map.empty)
        def op(m1: Money, m2: Money): Money = Money(m2.m.foldLeft(m1.m) { (a, e) =>
            val (key, value) = e
            a.get(key).map(v => a + ((key, implicitly[Monoid[BigDecimal]].op(v, value)))).getOrElse(a + ((key, value)))
        })
    }


    /*************************************************
      * 统计模块接口的实现
      * */
    object Analytics extends Analytics[Balance, Money] {
        def sumBalances(balances: List[Balance])(implicit m: Monoid[Money]): Money =
            balances.foldLeft(m.zero) { (a, b) =>     // 从 Monoid.zero 开始运算，但是并不关心 zero 究竟是什么值
                m.op(a, creditBalance(b))             // 只调用 Monoid 合约的 op 接口.
        }
        private def creditBalance(b: Balance): Money = ??? //..
    }
}
