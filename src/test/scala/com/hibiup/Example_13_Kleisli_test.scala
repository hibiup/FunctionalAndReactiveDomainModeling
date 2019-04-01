package com.hibiup

import org.scalatest.FlatSpec

class Example_13_Kleisli_test extends FlatSpec{
    "Difference between Kleisli and flatmap" should "" in {
        import scalaz._
        import Scalaz._


        /** for-comprehension 帮助在定义时基于 Monad 实现组合抽象.*/
        trait test1 {
            def f1(i: Int): List[Int]
            def g1(j: Int): List[Int]

            // 基于 flatMap 的组合, 需要仔细考虑组合的方式,容易出错.
            def res(i:Int) = for{
                f <- f1(i)
                g <- g1(f)
            } yield(g)
        }

        // 在实现时带入值进行计算
        val t1 = new test1() {
            override def f1(i: Int): List[Int] = List(i, 2 * i)
            override def g1(j: Int): List[Int] = List(j * 2, j * 4)
        }
        import t1._
        assert(res(10) ==  List(20, 40, 40, 80))


        /** 基于 Kleisli 的组合 */
        trait test2 {
            import scalaz._
            import Scalaz._

            def f(i: Int): List[Int]
            def g(j: Int): List[Int]

            // Kleisli 定义组合更直观, 表达性更好, 更灵活
            val resK1 = Kleisli(f) >=> Kleisli(g)
            val resK2 = Kleisli(f) <=< Kleisli(g)
        }

        // 在实现时带入值进行计算
        val t2 = new test2() {
            override def f(i: Int): List[Int] = List(i, 2 * i)
            override def g(j: Int): List[Int] = List(j * 2, j * 4)
        }

        import t2._
        assert(resK1(10) ==  List(20, 40, 40, 80))
        assert(resK2(10) ==  List(20, 40, 40, 80))
    }
}
