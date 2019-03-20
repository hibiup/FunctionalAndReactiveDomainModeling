package com.hibiup

import org.scalatest.FlatSpec

class Example_7_Repository_With_Reader_test extends FlatSpec {
    "Transformer" should "" in {
        /** Transformer 类的定义：它有两个类型参数，和一个函数参数：
          *     第一个类型是输入的数据类型，第二个类型参数是输出的数据类型．
          *     函数参数是从Ｒ到Ａ的执行体.*/
        case class Transfer[R, A](run: R => A) {}

        /** 生成 Transfer 的时候需要将函数参数传入 */
        val t = Transfer[Int, String](i => i.toString)

        /** 使用的时候可以直接调用实例的函数参数 */
        println(t.run(1))
    }
}
