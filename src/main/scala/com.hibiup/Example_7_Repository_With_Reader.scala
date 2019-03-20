package com.hibiup

import java.util.{Calendar, Date}

import util.{Failure, Success, Try}
import collection.mutable.{Map => MMap}

/**
  * P102(127): 用 Reader 实现对数据库的组合调用
  */
object Example_7_Repository_With_Reader extends App{
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

    /*********************************
      * 1.3) 定义 Reader（也可以直接使用 Cats 的 ReaderT）.
      *
      * 相对于上一个例子，我们用隐式转换为 AccountRepository => Try 添加 map 和 flatMap，另一个更值得推荐的方法是明确定义一个 Monad
      * 用来负责对某个有副作用的源读取执行结果。
      *
      * Reader 的语义是：从（副）作用源 T 中得到输出类型 A. 所以它有两个类型参数，和一个函数参数：
      *     第一个类型是（副）作用源的数据类型，第二个类型参数是后面函数参数的输出类型．
      *     函数参数是从（副）作用源中读取数据并转换成输出的函数体.
      *
      * map 和 flatMap 函数实现 Applicitive 和 Monad，接受一个类型参数和一个函数参数
      *     类型参数是新的 Reader 的第二个类型参数的数据类型.
      *     函数参数调用当前 run 得到当前 Reader 的输出类型Ａ然后转换成新的 Reader 的输出类型 Ｂ．同时这个函数参数也就成了新的 Reader
      *     的参数 run 的函数体(的一部分)
      */
    final case class Reader[R, A](run: R => A) {
        def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))
        def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(run(r)).run(r))
    }

    /** 1.4) Service，返回 Reader */
    trait AccountService[Account, Amount, Balance] {
        /** 返回 Reader[AccountRepository, Try[Account]] transformer */
        def open(no: String, name: String, openingDate: Option[Date]): Reader[AccountRepository, Try[Account]]
        def close(no: String, closeDate: Option[Date]): Reader[AccountRepository, Try[Account]]
        def debit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
        def credit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
        def balance(no: String): Reader[AccountRepository, Try[Balance]]
    }


    /*********************************
      * 2) 通过 for-comprehension 以实现函数式组合调用
      */
    /** 2.1）函数式组合调用 */
    // 获得 Service 的合约接口 (在编写组合的时候，可以没有实现，函数式编程是面向合约的，并不会因为没有函数体而失败)，
    // 实现可以被推迟到调用组合之前（第4步）再提供。
    import AccountService._

    // 函数组合：for-comprehension 从每个方法中得到 Reader，而不是直接得到结果。然后再（隐式）执行flatMap和map来获取结果
    // (参见以下 4）代数实现的说明）
    def op(no: String):Reader[AccountRepository, Try[Balance]] = for {
        _ <- credit(no, BigDecimal(100))   // 结果被更新到Repository: { "1" -> Account("1", Balance(100)) }，返回 Success|Failure
        _ <- credit(no, BigDecimal(300))   // 结果被更新到Repository: { "1" -> Account("1", Balance(400)) }，返回 Success|Failure
        _ <- debit(no, BigDecimal(160))    // 结果被更新到Repository: { "1" -> Account("1", Balance(240)) }，返回 Success|Failure
        b <- balance(no)                   // 返回结果： Success(Balance(240))，或　Failure(...)
    } yield b


    /*********************************
      * 4）代数实现。将原本的函数执行体作为 Reader 的函数参数体来构建 Reader 而不是立即执行它，也就是将函数封装起来以延迟它的执行。
      *    被延迟执行的函数直到被 for-comprehension 以 Monadic 方式得到执行，从而实现了对执行过程的控制（参见 2.1）。
      * */
    /** 4.1）实现 Service  */
    case object AccountService extends AccountService[Account, Amount, Balance] {
        override def open(no: String, name: String, openingDate: Option[Date]): Reader[AccountRepository, Try[Account]] = Reader { repo: AccountRepository =>
            /** 因为返回的是 Reader[AccountRepository, Try[Account]]，而不是执行结果，所以以下 Reader 的执行过程（实现）
              * 可以一直推迟到 run 被调用之前再提供。设计阶段可以用 ??? 替代。（以下各方法同理。） */
            repo.query(no) match {
                case Success(Some(a)) => Failure(new Exception(s"Already existing account with no $no"))
                case Success(None) =>
                    if (no.isEmpty || name.isEmpty) Failure(new Exception(s"Account no or name cannot be blank") )
                    else if (openingDate.getOrElse(today) before today) Failure(new Exception(s"Cannot open account in the past"))
                    else repo.store(Account(no, name, openingDate.getOrElse(today)))
                case Failure(ex) => Failure(new Exception(s"Failed to open account $no: $name", ex))
            }
        }

        override def close(no: String, closeDate: Option[Date]): Reader[AccountRepository, Try[Account]] = Reader { repo: AccountRepository =>
            repo.query(no) match {
                case Success(Some(a)) =>
                    if (closeDate.getOrElse(today) before a.dateOfOpening)
                        Failure(new Exception(s"Close date $closeDate cannot be before opening date ${a.dateOfOpening}"))
                    else repo.store(a.copy(dateOfClosing = closeDate))
                case Success(None) => Failure(new Exception(s"Account not found with $no"))
                case Failure(ex) => Failure(new Exception(s"Fail in closing account $no", ex))
            }
        }

        override def debit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]] = Reader { repo: AccountRepository =>
            repo.query(no) match {
                case Success(Some(a)) =>
                    if (a.balance.amount < amount) Failure(new Exception("Insufficient balance"))
                    else repo.store(a.copy(balance = Balance(a.balance.amount - amount)))
                case Success(None) => Failure(new Exception(s"Account not found with $no"))
                case Failure(ex) => Failure(new Exception(s"Fail in debit from $no amount $amount", ex))
            }
        }

        override def credit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]] = Reader { repo: AccountRepository =>
            repo.query(no) match {
                case Success(Some(a)) => repo.store(a.copy(balance = Balance(a.balance.amount + amount)))
                case Success(None) => Failure(new Exception(s"Account not found with $no"))
                case Failure(ex) => Failure(new Exception(s"Fail in credit to $no amount $amount", ex))
            }
        }

        override def balance(no: String): Reader[AccountRepository, Try[Balance]] =
            Reader((repo: AccountRepository) => repo.balance(no))
    }


    /*********************************
      * 5) 调用函数组合执行代码
      * */
    val repo = new AccountRepository()   // 初始化存储库

    // 调用 op 方法(2.1)得到 Reader 实例(AccountService.balance)。
    op("1")
            /** 然后调用实例的 run 函数(4.1)传入存储库得到输出。run 的 body 到此时才被需求。
              * 也就是说如果在此之前函数体是???，那么此刻需要一个实现了，此时整个过程也接近尾部，也就是所谓的“计算的边缘”。
              * Reader monad 通过这种方式将副作用计算退离了核心逻辑。*/
            .run(repo) match {
        case Success(Balance(b)) => println(s"Balance: $$$b")
        case _ => ???
    }

    // 记录不存在，返回失败
    op("2").run(repo)  match {
        case Failure(e) => println(s"Exception: $e")
        case _ => ???
    }
}
