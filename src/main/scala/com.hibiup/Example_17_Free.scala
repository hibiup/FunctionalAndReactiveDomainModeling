package com.hibiup.example_17

/**
  * 五个步骤实现　Free Monad，前四个步骤都围绕代数模型展开，第五个步骤提交业务实现逻辑：
  *
  * 1）  定义代表服务输入输出的 ADTs
  * 2）  将服务 ADTs 的 Coproduct 提升为 Free[Coproduct, _]
  * 3）  将服务 ADTs 加载到 Free[Coproduct, _]
  * 3-1）并基于 Free[Coproduct, _] 实现业务组合
  * 4）  直到 the end of the world, 提交业务实现逻辑。
  *
  * */

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

package Example_17_Free {
    import java.util.{Calendar, Date}

    import com.hibiup.example_17.Example_17_Free.model.AccountRepoF
    import scalaz.Free

    /********************************************/
    trait ADTs {
        type Account

        /**
          * 1) 定义操作的代数类型．
          *
          * 注意！不是定义代数，而是定义 ADT !!
          * */
        // 定义一个 Coproduct
        trait AccountRepo[+A]     // +A 是返回类型

        // 定义 Product
        case class Store(account: Account) extends AccountRepo[Unit]     // 对应: def store: Account => Unit
        case class Delete(no: String) extends AccountRepo[Unit]          // 对应: def delete: String => Unit
        case class Query(no: String) extends AccountRepo[Account]        // 对应: def query: String => Account

        /** 2) 得到 Coproduct 的 Free Monad*/
        // Free 是 Scalaz 提供的一个 Monad, Coproduct 被作为它的第一参数传入。得到一个基于这个Coproduct的 Free Monad
        type AccountRepoF[A] = Free[AccountRepo, A]
    }

    /********************************************/
    object model extends ADTs {
        // 实现 ADT, 提供 type Account 的具体定义
        val today = Calendar.getInstance.getTime

        type Amount = BigDecimal
        case class Account(no:String, name:String, openDate:Date=today, dateOfClosing:Option[Date]=None, balance:Amount=BigDecimal(0))
    }

    trait AccountRepository
    {
        import model._

        /** 3)用 scalaz.Free.liftF 函数将代表运算的各个 ADT 装载（lift）到这个 Free Monad(AccountRepoF) 中去。 */
        // 注意：不是接实现业务运算，而是用 Free.liftF({ADT}) 来替代业务运算。
        def store(account: Account): AccountRepoF[Unit] = Free.liftF(Store(account))
        def delete(no:String): AccountRepoF[Unit] = Free.liftF(Delete(no))
        def query(no:String):AccountRepoF[Account] = Free.liftF(Query(no))

        // Free monad 也可以组合成更复杂的运算
        def update(no: String, f: Account => Account): AccountRepoF[Unit] = for {
            account <- query(no)
            _ <- store(f(account))
        } yield ()


        def updateBalance(no: String, amount: Amount,
                          f: (Account, Amount) => Account): AccountRepoF[Unit] = for {
            account <- query(no)     // query 返回 AccountRepoF[Account] 类型，for-comprehension 提取出 Account
            _ <- store(f(account, amount))
        } yield ()
    }

    // 实现一个 AccountRepository 实例.
    object AccountRepository extends AccountRepository

    /********************************************/
    /** 3-1) 使用 monad 组合的强大功能，您现在可以利用通过 AccountRepository 模块发布的 API 实现服务端 DSL */
    // 本例中并没有用到 AccountService, 可以直接跳到 5)
    trait AccountService {
        import model._
        import AccountRepository._   // 明确地引入

        // 辅助函数
        def newAccount(no:String, name:String, openDate:Date):Account
        def updateClosingAccount(account:Account): Account

        // DSL:
        // 和之前一样，并不直接实现运算，而是用 lift 组合来代替。
        def open(no: String, name: String, openingDate: Option[Date]): AccountRepoF[Account]  = for {
            _ <- store(newAccount(no, name, openingDate.get))
            new_open <- query(no)
        } yield new_open

        def close(no: String) = for {
            _ <- update(no, updateClosingAccount)
            closed <- query(no)
        } yield closed
    }

    // 实现一个 AccountService 实例，虽然本例中没有用到
    object AccountService extends AccountService {
        import model._

        override def newAccount(no: String, name: String, openDate: Date): Account = Account(no, name, openDate)
        override def updateClosingAccount(account: Account): Account = account.copy(dateOfClosing = Option(today))
    }

    /********************************************/
    /** 4) 定义 Interpreter,　将 Free Monad 作为参数传入
      *
      * 与 Kleisli 的例子不同，这个 Interpreter 不继承 AccountRepository，相反它将 AccountRepository 中的函数(或由其衍生的复合函数)
      * 作为参数，然后将运算应用在之上．
      * */
    trait AccountRepositoryInterpreter[M[_]] {
        // 将一个 Free Monad 映射到一个带有业务运算的 Task Monad
        def apply[A](action: AccountRepoF[A]): M[A]
    }

    /** 实现 Interpreter. 实现映射和映射之后的业务运算.
      * */
    case class AccountRepoMutableInterpreter() extends AccountRepositoryInterpreter[Task] {
        import model._
        import scala.collection.mutable.{ Map => MMap }

        // 基于内存的数据库
        private val table: MMap[String, Account] = MMap.empty[String, Account]

        /** 将代表运算的 ADT 从 Free Monad 中取出交给 step(ADT ~> Task). route 根据 ADT 实现路由 */
        override def apply[A](action: AccountRepoF[A]): Task[A] = action.foldMap(route)

        /** 5) 实现业务计算 */
        // route 获得 Free 内的装载 ADT(AccountRepo)，将它转成 M (Task[A]).
        // Task[A] 是 Scalaz 提供的 Future[A] 替代。
        // ~> 是 Scalaz 提供的 Transformation 运算，因为 apply[A] 的参数 A 需要适应性透明传递
        private val route: AccountRepo ~> Task = new (AccountRepo ~> Task) {
            override def apply[A](action: AccountRepo[A]): Task[A] = action match {  //
                case Query(no) =>
                    table.get(no)
                            .map { a => Task.now(a) }
                            .getOrElse {
                                Task.fail(new RuntimeException(s"Account no $no not found"))
                            }
                case Store(account) =>
                    Task.now(table += ((account.no, account))).void
                case Delete(no) =>
                    Task.now(table -= no).void
            }
        }
    }


    /** 使用 ******************************************/
    object Client extends App {
        import model._
        import AccountRepository._

        // compF 是一个由 AccountRepository 衍生出静态的 Free monad 组合，它将被交给 AccountRepositoryInterpreter
        def compF: Account => AccountRepoF[Unit] = account => for {
            a <- query(account.no)
            _ <- store(a.copy(balance = BigDecimal(1000)))
            _ <- delete(a.no)
        } yield a

        /** 将 Free Monad 转递给 Interpreter*/
        val interpreter = AccountRepoMutableInterpreter()   // 最好隐式获得
        val compInter: Task[_] = interpreter(compF(Account("a-123", "John K")))

        /** 执行 interpreter */
        compInter.unsafePerformAsync{
            case \/-(a) => println(a)
            case -\/(e) => println(e)
        }
    }
}
