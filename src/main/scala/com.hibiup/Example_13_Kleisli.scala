package com.hibiup

import scalaz.Kleisli

object Example_13_Kleisli {
    /**
      * 通过统一语言定义出 Algebra 类型
      *
      * 注意每个代数类型之间的输入和输出具有连贯性
      * */
    trait Trading_Algebra[Account, Market, Order, ClientOrder, Execution, Trade] {
        def clientOrders: ClientOrder => List[Order]                  // 输出 Order 是下一个代数类型的输入
        def execute: Market => Account => Order => List[Execution]    // 输出 Execution 是下一个的输入
        def allocate: List[Account] => Execution => List[Trade]       // 上一个的输出本类型的输入
    }

    /** 根据代数类型设计函数界面和返回值
      *
      * 保持返回值是 Monad
      *
      * */
    trait Trading_Algebra_to_Function[Account, Market, Order, ClientOrder, Execution, Trade] {
        def clientOrders: ClientOrder => List[Order]                  //
        def execute(m: Market, a: Account): Order => List[Execution]  // 将额外的输入作为函数参数，保持上一个函数的输出作为本函数的 curry 输入条件。
        def allocate(as: List[Account]): Execution => List[Trade]     // 同样上一个函数的输出作为本函数的 curry 输入条件。
    }

    /** 只要返回值是 Monad，并且上一个函数的输出可以作为下一个函数的输入（curry），那么就可以定义连续的 Kleisli 箭头 */
    trait Trading_Function_to_Kleisli[Account, Market, Order, ClientOrder, Execution, Trade] {
        def clientOrders: Kleisli[List, ClientOrder, Order]            // 根据函数关系设计 Kleisli
        def execute(m: Market, a: Account): Kleisli[List, Order, Execution]
        def allocate(as: List[Account]): Kleisli[List, Execution, Trade]

        /** Kleisli 组合 */
        def tradeGeneration(market: Market,
                            broker: Account,
                            clientAccounts: List[Account]) = {
            clientOrders andThen
                    execute(market, broker) andThen
                    allocate(clientAccounts)
        }
    }
}
