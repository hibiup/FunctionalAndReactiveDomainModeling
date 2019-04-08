package com.hibiup

import scalaz._
import Scalaz._

package example_18 {
    /**
      * 例一：用普通 Either(Scalaz.\/) 作为容器，没有嵌套容器。
      * */
    object None_Transformer_None_Future extends App {
        type Valid[A] = Exception \/ A

        trait Design {
            def i2f: Kleisli[Valid, Int, BigDecimal]
            def f2s: Kleisli[Valid, BigDecimal, String]
            def comp: Kleisli[Valid, Int, String] = i2f andThen f2s
        }

        object DesignInterpreter extends Design {
            override def i2f: Kleisli[Valid, Int, BigDecimal] = Kleisli { i =>
                println(s"[Thread-${Thread.currentThread().getId}] - i2f")
                if (i >= 0) BigDecimal(i).right else (new RuntimeException("Input is smaller then 0")).left
            }

            override def f2s: Kleisli[Valid, BigDecimal, String] = Kleisli { f =>
                println(s"[Thread-${Thread.currentThread().getId}] - f2s")
                f.toString().right
            }
        }

        import DesignInterpreter._

        println(s"[Thread-${Thread.currentThread().getId}] - main")
        comp(-1) match {
            case \/-(s) => println(s"Finally we got: ${s}")
            case -\/(e) => println(e.getMessage)
        }
        println(s"[Thread-${Thread.currentThread().getId}] - main")
    }

    /**
      * 例二：用 Future 作为 Either 的外层容器（Future[Either]）
      * */
    object Future_Without_Transformer extends App {
        import scala.concurrent.Future
        implicit val ec = scala.concurrent.ExecutionContext.global

        /** 定义 Either 为返回值的容器 */
        type Valid[A] = Exception \/ A

        trait Design {
            /** Kleisli：用 Future 作为返回值容器。输入值和输出值包裹在内层 Either 中，这需要 interpreter 自己去解包。 */
            def i2f: Kleisli[Future, Int, Valid[BigDecimal]]
            def f2s: Kleisli[Future, Valid[BigDecimal], Valid[String]]
            def comp: Kleisli[Future, Int, Valid[String]] = i2f andThen f2s
        }

        object DesignInterpreter extends Design {
            override def i2f: Kleisli[Future, Int, Valid[BigDecimal]] = Kleisli { i =>
                Future {
                    println(s"[Thread-${Thread.currentThread().getId}] - i2f")
                    if (i >= 0) (BigDecimal(i)).right else (new RuntimeException("Input is smaller then 0")).left
                }
            }

            /** 不使用 Transformer 则需要将 f:Valid 解出，然后再按 Future -> Valid 打包回去．
              *
              * 不仅需要拆包，并且会导致以下 Future 必定会被执行，即便前一步骤返回异常（因为不能提前判断）。
              * */
            override def f2s: Kleisli[Future, Valid[BigDecimal], Valid[String]] = Kleisli {f:Valid[_] =>
                println(s"[Thread-${Thread.currentThread().getId}] - f2s in the same thread of i2f's Future")
                // 解出 Either.
                f match {
                    case \/-(s) => Future { // 重新打包成 Future[Either]
                        println(s"[Thread-${Thread.currentThread().getId}] - f2s in Future")
                        s.toString.right
                    }
                    case -\/(e) => Future {
                        println(s"[Thread-${Thread.currentThread().getId}] - f2s in Future <- 不应该执行")
                        e.left
                    }
                }
            }
        }

        import DesignInterpreter._
        import scala.concurrent.Await
        import scala.concurrent.duration._

        println(s"[Thread-${Thread.currentThread().getId}] - main")
        Await.result(comp(2), 10 seconds)match {
            case \/-(s) => println(s"[Thread-${Thread.currentThread().getId}]: Finally we got: ${s}")
            case -\/(e) => println(s"[Thread-${Thread.currentThread().getId}]: ${e.getMessage}")
        }
        println(s"[Thread-${Thread.currentThread().getId}] - main")

        println(s"[Thread-${Thread.currentThread().getId}] - i2f 失败并不会提前终止 f2s")
        Await.result(comp(-2), 10 seconds)match {
            case \/-(s) => println(s"[Thread-${Thread.currentThread().getId}]: Finally we got: ${s}")
            case -\/(e) => println(s"[Thread-${Thread.currentThread().getId}]: ${e.getMessage}")
        }

        println(s"[Thread-${Thread.currentThread().getId}] - main")
    }

    /**
      * 例三：用 EitherT 将内层返回值 Either 传送到 Future 的外层（EitherT[Future[Either]]）
      * */
    object Future_With_Transformer extends App {
        import scala.concurrent.Future
        implicit val ec = scala.concurrent.ExecutionContext.global

        /** 将 Future 定义在 EitherT 里面，但是最里层依然是 Either. */
        type Valid[A] = EitherT[Future, Exception, A]

        trait Design {
            /** Kleisli 的定义：用 EitherT 作为容器，而非 Future
              *
              *  这种定义方式相比于前一种，只是在 Future 外再套了一层 Transformer, 并用它取代 Future 作为 Kleisli 的返回值容器
              *  如此一来，输入值和输出值，都可以直接定义成原始类型，以方便 interpreter 处理。
              * */
            def i2f: Kleisli[Valid, Int, BigDecimal]
            def f2s: Kleisli[Valid, BigDecimal, String]
            def comp: Kleisli[Valid, Int, String] = i2f andThen f2s

            /** 与 Kleisli 不同, for-comprehension 属于实现时手段, 它会暴露参数传递的过程 */
            def compFor: Int => Valid[String] = i => for {
                f <- i2f(i)
                s <- f2s(f)
            } yield s
        }

        object DesignInterpreter extends Design {
            override def i2f: Kleisli[Valid, Int, BigDecimal] = Kleisli { i =>
                println(s"[Thread-${Thread.currentThread().getId}] - i2f")
                /** 返回的时候需要一层一层按顺序打包回 EitherT[Future[Either]]　*/
                EitherT {
                    Future {
                        if (i >= 0) BigDecimal(i).right
                        else (new RuntimeException(s"[Thread-${Thread.currentThread().getId}] - i2f - in Future: Input is smaller then 0")).left
                    }
                }
            }

            /** 直接得到 f:BigDecimal, 相比上一种方法不需要对输入值进行解包，降低了输入处理的复杂度。
              *
              * 并且因为提前拆包，因此如果前一步骤返回异常，以下过程得以避免执行.
              * */
            override def f2s: Kleisli[Valid, BigDecimal, String] = Kleisli { f:BigDecimal =>     // 不必对 f 解包，直接就得到输入值。
                println(s"[Thread-${Thread.currentThread().getId}] - f2s - in the same thread of i2f's Future  <- 如果前一步骤失败, 将被避免执行")
                EitherT {
                    Future {
                        println(s"[Thread-${Thread.currentThread().getId}] - f2s - in Future")
                        f.toString.right     // 虽然输入简化了，但是返回值还是要打包成 Transformer 支持的类型
                    }
                }
            }
        }

        import DesignInterpreter._
        import scala.concurrent.Await
        import scala.concurrent.duration._

        println(s"[Thread-${Thread.currentThread().getId}] === main: 测试 Kleisli ===")
        comp(2) match {
            case EitherT(f) => Await.result(f.map {
                case \/-(s) => println(s"[Thread-${Thread.currentThread().getId}]: Finally we got: ${s}")
                case -\/(e) => println(s"[Thread-${Thread.currentThread().getId}]: ${e.getMessage}")
            }, 10 seconds)
        }
        println(s"[Thread-${Thread.currentThread().getId}] - main")

        println(s"[Thread-${Thread.currentThread().getId}] - i2f 失败将避免执行 f2s")
        comp(-2) match {
            case EitherT(f) => Await.result(f.map {
                case \/-(s) => println(s"[Thread-${Thread.currentThread().getId}]: Finally we got: ${s}")
                case -\/(e) => println(s"[Thread-${Thread.currentThread().getId}]: ${e.getMessage}")
            }, 10 seconds)
        }

        println(s"[Thread-${Thread.currentThread().getId}] main === 测试 for-comprehension ===")
        compFor(2) match {
            case EitherT(f) => Await.result(f.map {
                case \/-(s) => println(s"[Thread-${Thread.currentThread().getId}]: Finally we got: ${s}")
                case -\/(e) => println(s"[Thread-${Thread.currentThread().getId}]: ${e.getMessage}")
            }, 10 seconds)
        }
        println(s"[Thread-${Thread.currentThread().getId}] - main")
        compFor(-2) match {
            case EitherT(f) => Await.result(f.map {
                case \/-(s) => println(s"[Thread-${Thread.currentThread().getId}]: Finally we got: ${s}")
                case -\/(e) => println(s"[Thread-${Thread.currentThread().getId}]: ${e.getMessage}")
            }, 10 seconds)
        }
    }

    /**
      * 例四：不正确的用法，需要自己实现 Bind[EitherT]
      * */
    object Future_A_Transformer extends App {
        import scala.concurrent.Future
        implicit val ec = scala.concurrent.ExecutionContext.global

        /** 定义 Either 为返回值的容器 */
        type Valid[A] = Exception \/ A
        type FutureValid[A] = Future[Valid[A]]   // 将 Future 作为返回类型

        trait Design {
            //def i2f: Kleisli[EitherT, Int, FutureValid[BigDecimal]]
            //def f2s: Kleisli[EitherT, FutureValid[BigDecimal], FutureValid[String]]

            /** 没有缺省的 Bind[EitherT], 无法实现 Compose */
            // def comp: Kleisli[EitherT, Int, FutureValid[String]] = i2f andThen f2s
        }

        /*object DesignInterpreter extends Design {
            override def i2f: Kleisli[EitherT, Int, FutureValid[BigDecimal]] = ???
            override def f2s: Kleisli[EitherT, FutureValid[BigDecimal], FutureValid[String]] = ???
        }*/
    }
}