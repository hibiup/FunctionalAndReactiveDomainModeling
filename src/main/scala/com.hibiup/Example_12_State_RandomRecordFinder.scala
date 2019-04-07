package com.hibiup

import java.util.{Calendar, Date}

import scala.util.{Failure, Success, Try}
import collection.mutable.{Map => MMap}

/**
  * p133(158)
  *
  * Scalaz 提供了一个代替 while 循环的 whileM_ Monad，定义如下：
  *    trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  *        ...
  *        def whileM_[A](p: F[Boolean], body: => F[A]): F[Unit]   // 返回父类的参数型 F
  *        ...
  *    }
  * 只要布尔表达式返回 true，就会重复执行 body。
  *
  * 本例演示根据随机数从（内存）数据库中随机捡取记录，数据库只有一条 Account 编号为 1 的记录，随机数范围是 1~9，只有命中 1 时返回．
  * */

object Example_12_State_RandomRecordFinder extends App {
    /***********************************/
    /** 建模 */
    val today = Calendar.getInstance.getTime

    // 数据库 Models
    type Amount = BigDecimal
    case class Balance(amount: Amount = 0)
    case class Account(no: Int, name: String, dateOfOpening: Date = today, dateOfClosing: Option[Date] = None,
                       balance: Balance = Balance(0))

    trait Repository[A, IdType] {
        def query(id: IdType): Try[Option[A]]
        def store(a: A): Try[A]
    }

    class AccountRepository(records:MMap[Int, Account]) extends Repository[Account, Int]{
        lazy val repo = records

        def query(no: Int): Try[Option[Account]] = Success(repo.get(no))
        def store(a: Account): Try[Account] = {
            val r = repo += ((a.no, a))
            Success(a)
        }
        def balance(no: Int): Try[Balance] = query(no) match {
            case Success(Some(a)) => Success(a.balance)
            case Success(None) => Failure(new Exception(s"No account exists with no $no"))
            case Failure(ex) => Failure(ex)
        }
        def query(openedOn: Date): Try[Seq[Account]] = Success(repo.values.filter(_.dateOfOpening == openedOn).toSeq)
    }

    // 数据捡取器接口
    trait AccountFinder {
        def exists: Boolean
    }

    /** 生成 whileLoop State Monad 更合适的地方. 参考下面实现阶段的说明 */
    /*def getWhileLoop[F[_], A <: AccountFinder](f: => A)(implicit m: MonadState[F, A]) = {
        import m._
        whileM_(
            gets(!_.exists),
            modify(_ => f)  // 直接内部改变 S,这也是为什么这个 State 没有执行函数
        )
    }*/


    /***********************************/
    /** 实现时 */

    // 数据准备
    val r:AccountRepository = new AccountRepository(MMap[Int, Account](
        1->Account(1, "John"))
    )

    // 设计随机数据捡取器
    final class RandomAccountFinder(rep: AccountRepository) extends AccountFinder {
        val account = rep.query(scala.util.Random.nextInt(10))
        def exists: Boolean = account match {
            case Success(Some(a)) => true
            case _ => false
        }
    }

    // 将 RandomAccountFinder 转换成 State Monad. 如果没有执行函数, State Monad 可以只接受 S 类型参数.
    // StateT是 monad 变换器, 它在这里做的并不重要, 只是帮助我们忽略第二个参数.
    import scalaz.StateT
    implicit val RandomAccountFinderState =   // 如果 whileLoop 不是设计时需求的, 并非必须 implicit
        StateT.stateMonad[RandomAccountFinder]

    /** whileM_　是所有的 Monad 都具有的一个隐式植入函数, 它返回一个 State Monad.
      *
      * 注:这个 特定的 while 应该改到建模阶段. (参考建模阶段的 getWhileLoop 函数 ). 用以下指令替换
      * */
    import RandomAccountFinderState.{whileM_, gets, modify}
    // val whileLoop = getWhileLoop(new RandomAccountFinder(r))
    val whileLoop = whileM_(     // 返回 State Monad: whileM_
        /** gets 是 MonadState 的方法, 这里用于得到新 State 的 S 参数
          * 只要 !_.exists==true 就会一直执行，也就是只要找不到就循环到找到为止。 */
        gets(!_.exists),
        /** 这个也是 MonadState.modify. 和前例中的 StateFunctions.modify 不同, 这里用于得到新 State 的 A 参数 */
        modify(_ => new RandomAccountFinder(r))  // 直接内部改变 S,这也是为什么这个 State 没有执行函数
    )

    val gen = new RandomAccountFinder(r)    // 生成初始状态
    val rnd = whileLoop exec gen            // 传入初始状态给 RandomAccountFinderState 开始执行查找
    println(rnd.account)
}
