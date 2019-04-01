package com.hibiup

import scalaz._
import Scalaz._

object Example_13_Kleisli {
    /**
      * 例一：Stock market 交易 { p140-143}
      * */
    /**
      * Version 1：
      *
      * 通过统一语言定义出 Algebra 类型
      *
      * 注意每个代数类型之间的输入和输出具有连贯性
      * */
    trait Trading_Algebra {
        type Account
        type Market
        type Order
        type ClientOrder
        type Execution
        type Trade

        def clientOrders: ClientOrder => List[Order]                  // 下单：输出 Order 是下一个代数类型的输入
        def execute: Market => Account => Order => List[Execution]    // 执行：输出 Execution 是下一个的输入
        def allocate: List[Account] => Execution => List[Trade]       // 返回：上一个的输出本类型的输入
    }

    /**
      * Version 2:
      *
      * 根据代数类型设计函数界面和返回值
      *
      * 保持返回值是 Monad
      * */
    trait Trading_Algebra_to_Function[Account, Market, Order, ClientOrder, Execution, Trade] {  // 可以用类型参数代替 type
        def clientOrders: ClientOrder => List[Order]                  //
        def execute(m: Market, a: Account): Order => List[Execution]  // 将额外的输入作为函数参数，保持上一个函数的输出作为本函数的 curry 输入条件。
        def allocate(as: List[Account]): Execution => List[Trade]     // 同样上一个函数的输出作为本函数的 curry 输入条件。
    }

    /**
      * Version 3 - Option 1:
      *
      * 只要返回值是 Monad，并且上一个函数的输出可以作为下一个函数的输入（curry），那么就可以定义连续的 Kleisli 箭头
      *
      * [Cats:]    https://blog.ssanj.net/posts/2017-06-07-composing-monadic-functions-with-kleisli-arrows.html
      * [Scalaz:]  http://eed3si9n.com/learning-scalaz/Composing+monadic+functions.html
      * [Scalaz:]  https://underscore.io/blog/posts/2012/07/02/kleisli-arrows.html
      *
      * A => F[B]          //g
      *        B => F[C]   //f
      *
      * A => F[C]          //f compose g
      *
      * */
    trait Trading_Function_to_Kleisli[Account, Market, Order, ClientOrder, Execution, Trade] {
        def clientOrders: Kleisli[List, ClientOrder, Order]                  // 根据函数关系设计 Kleisli
        def execute(m: Market, a: Account): Kleisli[List, Order, Execution]  // 第一个是容器类型，第二个时输入类型，第三个是输出类型
        def allocate(as: List[Account]): Kleisli[List, Execution, Trade]

        /** Kleisli 组合 */
        def tradeGeneration(market: Market,
                            broker: Account,
                            clientAccounts: List[Account]) = {
            // import scalaz._; Scalaz._ 或 cats.data.Kleisli; cats.implicits._
            clientOrders >=>                      // >=> 等价于 andThen
                    execute(market, broker) andThen
                    allocate(clientAccounts)
        }
    }

    /**
      * Version 3 - Option 2:
      *
      * 也可以利用 Scalaz 或 Cats 的 Kleisli 函数将函数的类型转换成 Kleisli arrow
      * */
    trait Trading_Function_to_Kleisli_implicitly[Account, Market, Order, ClientOrder, Execution, Trade] {
        def clientOrders: ClientOrder => List[Order]                  // 保持函数签名的外观。
        def execute(m: Market, a: Account): Order => List[Execution]
        def allocate(as: List[Account]): Execution => List[Trade]

        def tradeGeneration(market: Market,
                            broker: Account,
                            clientAccounts: List[Account]) = {
            /** 转换成 Kleisli */
            val clientOrdersK = Kleisli(clientOrders)          // Kleisli[List, ClientOrder, Order]
            val executeK = Kleisli(execute(market, broker))    // Kleisli[List, Order, Execution]
            val allocateK = Kleisli(allocate(clientAccounts))  // Kleisli[List, Execution, Trade]

            clientOrdersK >=>   // >=> 等价于 andThen
                    executeK >=>
                    allocateK
        }
    }
}
