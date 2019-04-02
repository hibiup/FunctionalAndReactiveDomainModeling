package com.hibiup

import java.util.Date

/**
  * p131(156) 例子说明：
  *
  * 用 State Monad 来记录帐户（Account）的状态变更。Balances是所有帐户数据的集合，数据类型是一个 Map, 存储了帐户 AccountNo
  * 和帐户上的存款 Balance。Transaction 是交易记录，存储 AccountNo，交易额和交易日期。多条交易组成 List 数据类型。
  *
  * AccountService.updateBalance 通过输入 List[Transaction] 得到 State Monad
  * */
object Example_11_State_AccountBalances extends App{
    /***************************************
      * 建模时
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
        /**
          * updateBalance 接口的设计和 Monad 有异曲同工之处。它接受一个参数和一个函数。不同的是参数指向改变的条件变量。
          * 函数则是一个 Monoid，返回的是一个Monad（转变后的结果）。我们隐约可以感受到它的内部逻辑应该也与典型的 Monad
          * 方法很类似。
          * */
        def updateBalance(txns: List[Transaction])(implicit m:Monoid[Balances]): State[Balances, Unit]
    }

    object AccountService extends AccountService {
        def updateBalance(txns: List[Transaction])(implicit m: Monoid[Balances]): State[Balances, Unit] =
        /**
          * StateFunction.modify (import State.modify) 方法用来修改 State Monad。
          *
          * 输入参数是之前的"状态"值, 返回新的"状态"实例。
          *
          * 是的，和我们定义 updateBalance 时的感受一样 modify 就是 Monad 的一个“变种"。它也接受两个参数，不同的是它的第一个参数
          * 是容器值（就差直接传入 State 容器本身了），第二个参数是胶水代码（胶合传入的 Monoid），最终返回一个（State）容器。这不正
          * 是变相的 Monad 方法么？！）。
          **/
            modify { b: Balances =>
                /** 以 balances 作为初始值(第一次 a==b, 以后每次 a 都是 m.append 之后返回的 Balances Map), 用每一条交易记录
                  * 来过滤之 */
                txns.foldLeft(b) { (a, txn) =>
                    /**
                      * 真正的 update 发生在 BalancesMonoid 内。BalancesMonoid 由实现时提供。所以通过隐式获得 Balances 的
                      * Monoid,将它应用于交易处理。（Monad 负责抽象的组合，Monoid 负责具体的运算，所以 Monad 发生在设计时，Monoid
                      * 发生在实现时）
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
    /**
      * 实现 Balances update 运算。
      *
      * 将 Balances 实现为 Monoid，将运算隔离出来，在使用时用隐式注入
      * */
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
    import AccountService._
    /**
      * .exec(balances) 返回 S
      * .run(balance) 返回 (S, A), 可以 ._1 或 ._2 取出。
      * .eval(balance) 返回 A
      * */
    val new_balances = updateBalance(txns).run(balances)
    println(s"State：$new_balances")
}
