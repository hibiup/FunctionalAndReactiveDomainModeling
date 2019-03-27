package com.hibiup

import java.util.Date

object Example_11_State_Random extends App{
    /***************************************
      * 设计时
      * */
    type Balance = BigDecimal
    type AccountNo = String
    type Balances = Map[AccountNo, Balance]    // 用一个 Map 来存放所有帐户的 Balances
    case class Transaction(accountNo: AccountNo, amount: Balance, date: Date)

    // State Monad
    import scalaz.Monoid
    import scalaz.State
    import State._

    /**
      * 帐户数据 Balances 是需要维护的全局"状态", 用一个 State[Balances, Unit] 来保存, 通过输入交易记录来改变它
      *
      * State[S, A] 的两个类型参数:
      *   S: 状态值
      *   A: 修改状态的函数
      * */
    trait AccountService {
        def updateBalance(txns: List[Transaction])(implicit m:Monoid[Balances]): State[Balances, Unit]
    }

    object AccountService extends AccountService {
        def updateBalance(txns: List[Transaction])(implicit m: Monoid[Balances]): State[Balances, Unit] =
        /**
          * StateFunction.modify (import State.modify) 方法用来修改 State Monad
          *
          * 输入参数是之前的"状态"实例, 返回新的"状态"实例
          **/
            modify { b: Balances =>
                /** 以 balances 作为初始值(第一次 a==b, 以后每次 a 都是 m.append 之后返回的 Balances Map), 用每一条交易记录来过滤之 */
                txns.foldLeft(b) { (a, txn) =>
                    /**
                      * 真正的 update 发生在 BalancesMonoid 内。BalancesMonoid 由实现时提供。所以通过隐式获得 Balances的
                      * Monoid,将它应用于交易处理。
                      *
                      * 我们需要将 Balances 定义成一个Monoid。实现它的 append 函数（Scalaz 定义的 Monoid 接口）以实现状态更新，
                      * 这个函数接受两个 Balances 合并成一个。第一个参数是前一个状态，第二个参数是新增加的状态值（单条Transaction）
                      * 因为 Balances 是一个 Map，所以我们通过 transaction 记录生成 Map（也就是 Balances）。
                      **/
                    m.append(a, Map(txn.accountNo -> txn.amount))
                }
            }
    }


    /********************************
      * 实现时
      * */
    /** 将 Balances 实现为 Monoid，将运算隔离出来，在使用时用隐式实现 */
    implicit val BalancesMonoid: Monoid[Balances] = new Monoid[Balances] {
        override def zero: Balances = Map.empty
        override def append(a: Balances, f: => Balances): Balances = {
            f.flatMap(t =>
                a.get(t._1).map(v => v + t._2).map(v => a + Tuple2(t._1, v)).getOrElse(a)
            )
        }
    }

    // 准备帐户数据
    val balances: Balances = Map(
        "1" -> BigDecimal(100),
        "2" -> BigDecimal(2000),    // 不存在交易
        "3" -> BigDecimal(200)
    )

    // 准备交易记录
    val txns: List[Transaction] = List(
        Transaction("1", BigDecimal(100), new Date ),
        Transaction("3", BigDecimal(-50), new Date ),
        Transaction("5", BigDecimal(-50), new Date )    // 不存在的帐号
    )

    // 提交运算
    val new_balances = AccountService.updateBalance(txns).exec(balances)
    println(new_balances)
}
