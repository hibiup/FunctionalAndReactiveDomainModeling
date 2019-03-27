package com.hibiup

import org.scalatest.FlatSpec

class Example_9_Functor_test extends FlatSpec{
    "A Generic Functor" should "" in {
        /**
          * 1) Functor
          */
        object MyFunctor {
            // 定义 Functor 的界面
            trait Functor[F[_]] {
                def map[A, B](a: F[A])(f: A => B): F[B]
            }

            // 将 T 隐式转换成 Functor
            implicit class MyClassOps[A, T[_]](my: T[A]) {
                // 根据 Functor trait 的定义调取 map
                def map[B](f: A => B)(implicit functor: Functor[T]): T[B] = functor.map(my)(f)
            }
        }

        /**
          * 2）设计阶段
          */
        import MyFunctor._

        // 定义一个容器
        case class MyClass[T](d: T)

        // 实例化 MyClass 容器的 Functor，实现 map 函数体（每一种类型(MyClass)，都要实现一个）
        implicit val MyClassFunctor = new Functor[MyClass] {
            override def map[A, B](a: MyClass[A])(f: A => B): MyClass[B] = {
                // 针对 MyClass 的特定的算法
                val MyClass(m) = a
                MyClass(f(m))
            }
        }

        /** 3) 使用调用 */
        // 使用 MyClass Functor（此时需要 map 的函数体）
        val my = MyClass[Int](1)

        println(my.map("\"" + _.toString + "\""))
        println(my.map(i => if (i>0) true else false))
    }


    "Simplify Functor" should "" in {
        case class MyClass[T](d: T) {
            def map[B](f: T => B): MyClass[B] = MyClass(f(d))
        }

        val my = MyClass[Int](1)

        println(my.map("\"" + _.toString + "\""))
        println(my.map(i => if (i>0) true else false))
    }
}
