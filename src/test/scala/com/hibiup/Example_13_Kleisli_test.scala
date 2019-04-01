package com.hibiup

import org.scalatest.FlatSpec

class Example_13_Kleisli_test extends FlatSpec{
    "Difference between Kleisli and flatmap" should "" in {
        import scalaz._
        import Scalaz._

        /** 定义时无法直接基于 FlatMap 组合抽象.*/
        def f(i: Int) = List(i, 2 * i)
        def g(j: Int) = List(j * 2, j * 4)

        // 在实现时带入值进行组合
        val res1 = f(10).flatMap(g)
        assert(res1 ==  List(20, 40, 40, 80))

        /** 相比而言，Kleisli 在更高的层次上抽象了运算,可以允许我们在定义时就进行组合 */
        import scalaz._
        import Scalaz._
        val resK = Kleisli(f) >=> Kleisli(g)   // 在设计时实现组合. 也就是说 Kleisli 将 for-comprehension 的工作移到设计时完成

        // 实现时只需要带入值即可。
        assert(resK(10) ==  List(20, 40, 40, 80))
    }
}
