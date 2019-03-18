package com.hibiup

import java.util.{Calendar, Date}

import util.{Failure, Success, Try}
import collection.mutable.{Map => MMap}

/**
  * P99(124): 实现对数据库的组合调用
  */
object Example_7_Repository extends App {
    val today = Calendar.getInstance.getTime

    /*********************************
     * 1）定义代数抽象
     * */
    /** 1.1) 基本数据类型　*/
    type Amount = BigDecimal
    case class Balance(amount: Amount = 0)
    case class Account(no: String, name: String, dateOfOpening: Date = today, dateOfClosing: Option[Date] = None,
                       balance: Balance = Balance(0))

    /** 1.2) Repository */
    trait Repository[A, IdType] {
        def query(id: IdType): Try[Option[A]]
        def store(a: A): Try[A]
    }

    class AccountRepository extends Repository[Account, String]{
        // 初始化 Repository，并给一条初始记录
        lazy val repo = MMap[String, Account]("1"->Account("1", "John"))

        def query(no: String): Try[Option[Account]] = Success(repo.get(no))
        def store(a: Account): Try[Account] = {
            val r = repo += ((a.no, a))
            Success(a)
        }
        def balance(no: String): Try[Balance] = query(no) match {
            case Success(Some(a)) => Success(a.balance)
            case Success(None) => Failure(new Exception(s"No account exists with no $no"))
            case Failure(ex) => Failure(ex)
        }
        def query(openedOn: Date): Try[Seq[Account]] = Success(repo.values.filter(_.dateOfOpening == openedOn).toSeq)
    }

    /** 1.3) Service */
    trait AccountService[Account, Amount, Balance] {
        /** 返回 Function1[AccountRepository, Try[Account]] Lambda 表达式 */
        def open(no: String, name: String, openingDate: Option[Date]): AccountRepository => Try[Account]
        def close(no: String, closeDate: Option[Date]): AccountRepository => Try[Account]
        def debit(no: String, amount: Amount): AccountRepository => Try[Account]
        def credit(no: String, amount: Amount): AccountRepository => Try[Account]
        def balance(no: String): AccountRepository => Try[Balance]
    }


    /*********************************
      * 2) 通过 for-comprehension 以实现函数式组合调用
      */
    /** 2.1）函数式组合调用 */
    // 获得 Service 的合约接口 (在编写组合的时候，可以没有实现，函数式编程是面向合约的，并不会因为没有函数体而失败)，
    // 实现可以被推迟到调用组合之前（第4步）再提供。
    import AccountService._

    // for-comprehension 要求结果(包括中间结果)必须是 Monad（具有 map 和 flatMap），如果不是，要提供隐式转换。
    import Implicit._

    // 函数组合
    def op(no: String) = for {
        _ <- credit(no, BigDecimal(100))   // 结果被更新到Repository: { "1" -> Account("1", Balance(100)) }，返回 Success|Failure
        _ <- credit(no, BigDecimal(300))   // 结果被更新到Repository: { "1" -> Account("1", Balance(400)) }，返回 Success|Failure
        _ <- debit(no, BigDecimal(160))    // 结果被更新到Repository: { "1" -> Account("1", Balance(240)) }，返回 Success|Failure
        b <- balance(no)                   // 返回结果： Success(Balance(240))，或　Failure(...)
    } yield b


    /*********************************
      * 3）为了实现以上代数的组合调用，将一般函数 AccountRepository => Try[Account] 提升到 Monad　以支持从 for-comprehension　
      * 中返回。（可以用 Cats 简化以下代码．）
      */
    /** 3.1) 定义 Monad */
    object Syntax {
        //* 定义 Monad */
        trait Functor[F[_]] {
            def map[A, B](a: F[A])(f: A => B): F[B]
        }

        object Functor {
            def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]

            implicit def ListFunctor: Functor[List] = new Functor[List] {
                def map[A, B](a: List[A])(f: A => B): List[B] = a map f
            }

            implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
                def map[A, B](a: Option[A])(f: A => B): Option[B] = a map f
            }

            implicit def Tuple2Functor[A1]: Functor[({type f[x] = (A1, x)})#f] = new Functor[({type f[x] = (A1, x)})#f] {
                def map[A, B](a: (A1, A))(f: A => B): (A1, B) = (a._1, f(a._2))
            }

            implicit def Function1Functor[A1]: Functor[({type f[x] = A1 => x})#f] = new Functor[({type f[x] = A1 => x})#f] {
                def map[A, B](fa: A1 => A)(f: A => B): A1 => B = fa andThen f
            }

        }

        trait Monad[F[_]] extends Functor[F] {
            def unit[A](a: => A): F[A]
            def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
            def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
            def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
            def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
            def sequence[A](lma: List[F[A]]): F[List[A]] =
                lma.foldRight(unit(List[A]())) { (ma, mla) => println(s"from monad $ma"); map2(ma, mla)(_ :: _) }
            def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
                la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

        }

        object Monad {
            def apply[F[_] : Monad]: Monad[F] = implicitly[Monad[F]]

            implicit val optionMonad: Monad[Option] = new Monad[Option] {
                def unit[A](a: => A) = Some(a)
                override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
            }
            implicit val listMonad: Monad[List] = new Monad[List] {
                def unit[A](a: => A) = List(a)
                override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
            }

            implicit def function1Monad[A1]: Monad[({type f[x] = A1 => x})#f] = new Monad[({type f[x] = A1 => x})#f] {
                def unit[A](a: => A): A1 => A = (_: A1) => a
                override def flatMap[A, B](r: A1 => A)(f: A => A1 => B): A1 => B = (t: A1) => f(r(t))(t)
            }
        }
    }

    /** 3.2) 为 for-comprehension 中的返回值　AccountRepository => Try[Account] 隐式添加 map　和 flatMap 方法． */
    object Implicit {
        import Syntax._

        implicit class FunctorSyntax[F[_] : Functor, A](a: F[A]) {
            def map[B](f: A => B): F[B] = Functor[F].map(a)(f)
        }

        implicit class Function1FunctorSyntax[A1, A](a: A1 => A) {
            def map[B](f: A => B): A1 => B = Functor[({type f[x] = A1 => x})#f].map(a)(f)
        }

        implicit class MonadSyntax[M[_] : Monad, A](a: M[A]) {
            def unit[A](a: => A): M[A] = Monad[M].unit(a)
            def flatMap[B](f: A => M[B]): M[B] = Monad[M].flatMap(a)(f)
        }

        implicit class Function1MonadSyntax[A1, A](a: A1 => A) {
            def unit[A](a: => A): A1 => A = Monad[({type f[x] = A1 => x})#f].unit(a)
            def flatMap[B](f: A => A1 => B): A1 => B = Monad[({type f[x] = A1 => x})#f].flatMap(a)(f)
        }

    }

    /*********************************
      * 4）代数的实现
      * */
    /** 4.1）实现 Service  */
    case object AccountService extends AccountService[Account, Amount, Balance] {
        /** 返回 Function1[AccountRepository, Try[Account]] Lambda 表达式 */
        override def open(no: String, name: String, openingDate: Option[Date]): AccountRepository => Try[Account] = (repo: AccountRepository) =>
            repo.query(no) match {
                case Success(Some(a)) => Failure(new Exception(s"Already existing account with no $no"))
                case Success(None) =>
                    if (no.isEmpty || name.isEmpty) Failure(new Exception(s"Account no or name cannot be blank") )
                    else if (openingDate.getOrElse(today) before today) Failure(new Exception(s"Cannot open account in the past"))
                    else repo.store(Account(no, name, openingDate.getOrElse(today)))
                case Failure(ex) => Failure(new Exception(s"Failed to open account $no: $name", ex))
            }

        override def close(no: String, closeDate: Option[Date]): AccountRepository => Try[Account] = (repo: AccountRepository) =>
            repo.query(no) match {
                case Success(Some(a)) =>
                    if (closeDate.getOrElse(today) before a.dateOfOpening)
                        Failure(new Exception(s"Close date $closeDate cannot be before opening date ${a.dateOfOpening}"))
                    else repo.store(a.copy(dateOfClosing = closeDate))
                case Success(None) => Failure(new Exception(s"Account not found with $no"))
                case Failure(ex) => Failure(new Exception(s"Fail in closing account $no", ex))
            }

        override def debit(no: String, amount: Amount): AccountRepository => Try[Account] = (repo: AccountRepository) =>
            repo.query(no) match {
                case Success(Some(a)) =>
                    if (a.balance.amount < amount) Failure(new Exception("Insufficient balance"))
                    else repo.store(a.copy(balance = Balance(a.balance.amount - amount)))
                case Success(None) => Failure(new Exception(s"Account not found with $no"))
                case Failure(ex) => Failure(new Exception(s"Fail in debit from $no amount $amount", ex))
            }

        override def credit(no: String, amount: Amount): AccountRepository => Try[Account] = (repo: AccountRepository) =>
            repo.query(no) match {
                case Success(Some(a)) => repo.store(a.copy(balance = Balance(a.balance.amount + amount)))
                case Success(None) => Failure(new Exception(s"Account not found with $no"))
                case Failure(ex) => Failure(new Exception(s"Fail in credit to $no amount $amount", ex))
            }

        override def balance(no: String): AccountRepository => Try[Balance] =
            (repo: AccountRepository) => repo.balance(no)
    }

    /*********************************
      * 5) 调用函数组合执行代码
      * */
    val repo = new AccountRepository()   // 初始化存储库

    // 调用 op 方法并传入存储库
    op("1")(repo) match {
        case Success(Balance(b)) => println(s"Balance: $$$b")
        case _ => ???
    }

    // 记录不存在，返回失败
    op("2")(repo)  match {
        case Failure(e) => println(s"Exception: $e")
        case _ => ???
    }
}
