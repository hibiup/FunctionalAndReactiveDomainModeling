package com.hibiup

import org.scalatest.FlatSpec

import scala.util.{Success, Failure, Try}

class Example_4_Future_test extends FlatSpec{
    "Try exception within for-comprehension" should "not makes happy path jump out" in {
        /**
          * for-comprehension 不仅仅是个让代码看上去更像指令式编程的语法糖，它更重要的作用是分离程序中的"作用"和"副作用"，
          * 让程序始终保持在“作用”的轨迹上，将副作用的处理推迟到“作用”代码块之外，而不是让处理副作用的代码蔓延的到处都是。
          * 这同时体现了“Ａ发Ｂ导”的设计思想，也就是Ａ中产生的异常（副作用）由Ｂ来负责处理，而不是Ａ自己去负责。这提高了Ａ
          * 的纯洁度。
          * */

        val t1 =                        {println("So far so good"); Success(1)}
        val t2:Int => Try[Int] = _ =>   {println("Something bad gonna happen!"); Failure(new Throwable)}
        val t3:Int => Try[Int] = i =>   {println("You would never get here..."); Success(i)}

        /** １）for-comprehension 将 side effect 从 happy path 中分离出来，将副作用的处理一直推迟到代码块结束。*/
        val result = for {
            a <- t1
            /** ２）代码虽然将终止在这里，但是并不需要立即陷入异常处理，因此代码的逻辑依然保持在正常业务处理上（虽然接下去的代码不会再执行）。*/
            b <- t2(a)
            c <- t3(b)
        } yield c

        /** ３）在逻辑快结束后处理 side effect. */
        result match {
            case Success(_) => fail()
            case Failure(t) => println(t)
        }
    }
}
