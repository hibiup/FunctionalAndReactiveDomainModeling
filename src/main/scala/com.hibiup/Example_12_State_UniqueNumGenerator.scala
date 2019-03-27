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

object Example_12_State_UniqueNumGenerator extends App {
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

    // 根据随机数捡取记录
    final class RandomAccountFinder(rep: AccountRepository) {
        val no: Int = scala.util.Random.nextInt(10)
        def exists: Boolean = rep.query(no) match {
            case Success(Some(a)) => true
            case _ => false
        }
    }

    /***********************************/
    /** 实现时 */

    // 数据准备
    val r:AccountRepository = new AccountRepository(MMap[Int, Account](
        1->Account(1, "John"))
    )

    // 将随机数生成器 Generator 转换成 State Monad. 如果没有执行函数, State Monad 可以只接受 S 类型参数.
    import scalaz.StateT
    val RandomAccountFinderState = StateT.stateMonad[RandomAccountFinder]

    import RandomAccountFinderState.{whileM_, gets, modify}
    // 只要 !_.exists==true 就会一直执行，也就是只要找不到就循环到找到为止。返回 State Monad: whileM_
    val whileLoop = whileM_(     // whileM_　是 Monad 的一个隐式植入函数, 它返回一个新的 State Monad
        // gets 是 MonadState 的方法
        gets(!_.exists),
        // 这个也是 MonadState.modify. 和前例中的 StateFunctions.modify 不同
        modify(_ => new RandomAccountFinder(r))  // 直接内部改变 S,这也是为什么这个 State 没有执行函数
    )

    val gen = new RandomAccountFinder(r)    // 生成初始状态
    val rnd = whileLoop exec gen            // 传入初始状态给 RandomAccountFinderState 开始执行查找
    println(rnd.no)
}
