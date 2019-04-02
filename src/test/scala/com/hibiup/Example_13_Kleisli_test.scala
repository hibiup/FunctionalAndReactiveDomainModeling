package com.hibiup

import org.scalatest.FlatSpec

class Example_13_Kleisli_test extends FlatSpec{
    "Difference between Kleisli and flatmap" should "" in {
        /**
          * 例一：Kleisli 基本概念
          * */

        /** for-comprehension 帮助在定义时基于 Monad 实现组合抽象.*/
        trait Test1 {
            def fa(i: Int): List[Int]
            def ga(j: Int): List[Int]

            // 基于 flatMap 的组合, 需要仔细考虑组合的方式,容易出错.
            def res1(i:Int) = for{
                f <- fa(i)
                g <- ga(f)
            } yield(g)
        }

        // 在实现时带入值进行计算
        val t1 = new Test1() {
            override def fa(i: Int): List[Int] = List(i, 2 + i)
            override def ga(j: Int): List[Int] = List(j * 2, j * 4)
        }
        import t1._
        assert(res1(10) ==  List(20, 40, 24, 48))


        /** 基于 Kleisli 的组合 */
        trait Test2 {
            import scalaz._; import Scalaz._

            def fa(i: Int): List[Int]
            def ga(j: Int): List[Int]

            // Kleisli 定义组合更直观, 表达性更好, 更灵活
            val resK1 = Kleisli(fa) >=> Kleisli(ga)
            val resK2 = Kleisli(fa) <=< Kleisli(ga)
        }

        // 在实现时带入值进行计算
        val t2 = new Test2() {
            override def fa(i: Int): List[Int] = List(i, 2 + i)
            override def ga(j: Int): List[Int] = List(j * 2, j * 4)
        }

        import t2._
        assert(resK1(10) ==  List(20, 40, 24, 48))
        assert(resK2(10) ==  List(20, 22, 40, 42))
    }


    "Apply loan example" should "" in {
        import scalaz._
        import Scalaz._

        /**
          * 例二：利用 Kleisli 实现申请并审核贷款流程 {p144(170)}
          * */
        import java.util.Date
        import java.util.Calendar

        /** 定义 */
        trait FormAndProcess {
            type Status
            type Applied <: Status
            type Approved <: Status
            type Enriched <: Status

            // 让申请表保存有状态，以防流程设计中出现错误
            type LoanApplication[Status]
            type LoanApplied = LoanApplication[Applied]
            type LoanApproved = LoanApplication[Approved]
            type LoanEnriched = LoanApplication[Enriched]


            // 申请贷款函数 applyLoan 输出一个常规类型 LoanApplication
            def applyLoan(name: String, purpose: String): LoanApplied  // 申请

            // LoanApplied 将作为审核贷款函数 approve 和 enrich 的输入(利用状态来防止顺序错误)
            def approve: Kleisli[Option, LoanApplied, LoanApproved]    // 审核
            def enrich: Kleisli[Option, LoanApproved, LoanEnriched]    // 补全数据

            // 定义 approve 和 enrich 的 compose
            val op = approve andThen enrich
        }

        /** 实现 */
        object FormAndProcess extends FormAndProcess{
            case class Application[Status](loanNo: Option[Int], name: String, purpose: String, applyDate: Option[Date] )
            override type LoanApplication[Status] = Application[Status]

            // 实现算法
            override def applyLoan(name: String, purpose: String): LoanApplied = {
                Application[Applied](None, "John","For fun!", None)
            }

            // Scalaz 的 Macro 可能引起编译器误报
            override def approve: Kleisli[Option, LoanApplied, LoanApproved] = Kleisli{ la =>
                la.copy(
                    applyDate = Calendar.getInstance.getTime.some
                ).some.map(identity[LoanApproved])
            }

            override def enrich: Kleisli[Option, LoanApproved, LoanEnriched] = Kleisli{ la =>
                la.copy(
                    loanNo = scala.util.Random.nextInt(10).some
                ).some.map(identity[LoanEnriched])
            }
        }

        /** 使用 */
        import FormAndProcess._

        // 得到 LoanApplication 实例
        val la = applyLoan("John B Rich", "House Building")

        // 将 la 实例应用于 compose
        val res = op run la
        println(res)
    }

    "Logistic" should "" in {
        /**
          * 结合 Future 和 State Monad
          * */
        import scalaz._
        import Scalaz._

        import java.sql.Timestamp
        import java.util.Calendar

        import scala.concurrent.Future

        /** 设计 */
        trait Logistic {
            private[Logistic] type SenderT
            type XiaoMing <: SenderT
            type XiaoZhang <: SenderT
            type XiaoWang <: SenderT
            type XiaoLi <: SenderT

            private[Logistic] type DeliveryT[Sender]
            type DeliveriedOrFailureT[Sender] = Either[Exception, DeliveryT[Sender]]
            type XiaoMingDeliveryT = DeliveriedOrFailureT[XiaoMing]
            type XiaoZhangDeliveryT = DeliveriedOrFailureT[XiaoZhang]
            type XiaoWangDeliveryT = DeliveriedOrFailureT[XiaoWang]
            type XiaoLiDeliveryT = DeliveriedOrFailureT[XiaoLi]

            /** 前两个接口演示利用类型不同来约束执行顺序(同书中的例子), 所有的函数都以 Future 为最终容器。*/
            def XiaoMingDelivery: Kleisli[Future, Timestamp, XiaoMingDeliveryT]
            def XiaoZhangDelivery: Kleisli[Future, XiaoMingDeliveryT, List[DeliveriedOrFailureT[_]]]

            /** 后两个接口是一样的，演示利用 State Monad 来追踪状态。*/
            def XiaoWangDelivery: Kleisli[Future, List[DeliveriedOrFailureT[_]], List[DeliveriedOrFailureT[_]]]
            def XiaoLiDelivery: Kleisli[Future, List[DeliveriedOrFailureT[_]], List[DeliveriedOrFailureT[_]]]

            // For Bind[Future]
            import scalaz.std.scalaFuture._
            import scala.concurrent.ExecutionContext.global
            implicit val ec = global

            // Compose method
            val deliveryPackage: Kleisli[Future, Timestamp, List[DeliveriedOrFailureT[_]]] =
                XiaoMingDelivery >=> XiaoZhangDelivery >=> XiaoWangDelivery >=> XiaoLiDelivery
        }

        /** 实现　*/
        object Logistic extends Logistic{
            case class Delivery[SenderT](start:Timestamp, endTime:Option[Timestamp])
            override type DeliveryT[SenderT] = Delivery[SenderT]

            /** 定义一个 State Monad 来记录 XiaoWang 和 XiaoLi 的 Delivery 状态. */
            implicit def deliveryState[S <: SenderT] = State[List[DeliveriedOrFailureT[_]], DeliveriedOrFailureT[S]]{ s =>
                val t = s.last match {
                    case Right(r) => r.endTime match {
                        case Some(time) => {
                            Thread.sleep(1000)
                            val endTime = new Timestamp(Calendar.getInstance.getTime.getTime)
                            println(s"[Thread-${Thread.currentThread.getId}] Delivery: $endTime")
                            Right(Delivery[S](time, Option(endTime)))
                            // 没有实现错误（Left）的情况
                        }
                    }
                    case Left(e) => ??? // 没有实现 Left
                }
                (s :+ t, t)
            }

            override def XiaoMingDelivery: Kleisli[Future, Timestamp, XiaoMingDeliveryT] = Kleisli {t =>
                Future.successful {
                    // 任务必定开始，因此第一个必定是 successful
                    val state = implicitly[State[List[DeliveriedOrFailureT[_]], DeliveriedOrFailureT[XiaoWang]]]
                    // Start 的任务只有 endTime (送入 State Monad 后也就是 XiaoMing 的 startTime )
                    state.exec(List(Right(Delivery[XiaoMing](null, Option(t)))))
                            .last.asInstanceOf[XiaoMingDeliveryT]  // 执行完后取出 XiaoMingDeliveryT 交给下一个（XiaoZhange）处理
                }
            }

            override def XiaoZhangDelivery: Kleisli[Future, XiaoMingDeliveryT, List[DeliveriedOrFailureT[_]]] = Kleisli {
                // 本例中 XiaoZhang 直接处理 XiaoMingDeliveryT 没有用 State 来 handle Left or Right，因此。。。
                case d@Right(r) => r.endTime match {
                    case Some(t) => Future.successful {
                        val state = implicitly[State[List[DeliveriedOrFailureT[_]], DeliveriedOrFailureT[XiaoZhang]]]
                        state.exec(List(d))
                    }
                }
                case _@Left(e) => ???
            }

            // XiaoWang 和 XiaoLi 的状态
            override def XiaoWangDelivery: Kleisli[Future, List[DeliveriedOrFailureT[_]], List[DeliveriedOrFailureT[_]]] = Kleisli {l =>
                Future {
                    val state = implicitly[State[List[DeliveriedOrFailureT[_]], DeliveriedOrFailureT[XiaoWang]]]
                    state.exec(l)
                }
            }

            override def XiaoLiDelivery: Kleisli[Future, List[DeliveriedOrFailureT[_]], List[DeliveriedOrFailureT[_]]] = Kleisli {l =>
                Future {
                    val state = implicitly[State[List[DeliveriedOrFailureT[_]], DeliveriedOrFailureT[XiaoLi]]]
                    state.exec(l)
                }
            }
        }

        /** 使用 */
        import Logistic._
        import scala.concurrent.Await
        import scala.concurrent.duration._

        val start = new Timestamp(Calendar.getInstance.getTime.getTime)
        println(s"[Thread-${Thread.currentThread.getId}] Start: $start")
        val result = Await.result(deliveryPackage(start), Duration.Inf)
        println(result)
    }
}
