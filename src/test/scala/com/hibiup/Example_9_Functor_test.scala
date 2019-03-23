package com.hibiup

import org.scalatest.FlatSpec

class Example_9_Functor_test extends FlatSpec{
    "A Generic Functor" should "" in {
        /**
          * 1) 设计阶段
          */
        object MyTest {
            // 定义一个容器
            case class MyClass[T](d: T)

            // 定义 Functor 的界面
            trait Functor[F[_]] {
                def map[A, B](a: F[A])(f: A => B): F[B]
            }

            // 将 MyClass 隐式转换成 Functor
            implicit class MyClassOps(my: MyClass[Int]) {
                // 根据 Functor trait 的定义调取 map
                def map[B](f: Int => B)(implicit functor: Functor[MyClass]): MyClass[B] = functor.map(my)(f)
            }
        }


        /**
          * 2）使用阶段
          */
        import MyTest._

        // 实例化 MyClass 容器的 Functor，实现 map 函数体（每一种类型(MyClass)，都要实现一个）
        implicit val MyClassFunctor = new Functor[MyClass] {
            override def map[A, B](a: MyClass[A])(f: A => B): MyClass[B] = {
                // 具有特定的算法
                val MyClass(m) = a
                MyClass(f(m))
            }
        }

        // 使用 MyClass Functor（此时需要 map 的函数体）
        val my = MyClass[Int](1)

        println(my.map("\"" + _.toString + "\""))
        println(my.map(i => if (i>0) true else false))
    }
}
