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

            private[Logistic] type DeliveryT[Sender]
            type XiaoMingDeliveryT = DeliveryT[XiaoMing]
            type XiaoZhangDeliveryT = DeliveryT[XiaoZhang]

            def XiaoMingDelivery: Kleisli[Future, Timestamp, XiaoMingDeliveryT]
            def XiaoZhangDelivery: Kleisli[Future, XiaoMingDeliveryT, XiaoZhangDeliveryT]

            // For Bind[Future]
            import scalaz.std.scalaFuture._
            import scala.concurrent.ExecutionContext.global
            implicit val ec = global

            // Compose method
            val deliveryPackage: Kleisli[Future, Timestamp, XiaoZhangDeliveryT] = XiaoMingDelivery >=> XiaoZhangDelivery
        }

        /** 实现　*/
        object Logistic extends Logistic{
            case class Delivery[SenderT](start:Timestamp,endTime:Option[Timestamp])
            override type DeliveryT[SenderT] = Delivery[SenderT]

            override def XiaoMingDelivery: Kleisli[Future, Timestamp, XiaoMingDeliveryT] = Kleisli {t =>
                Future {
                    println(s"[Thread-${Thread.currentThread.getId}] Start: $t")
                    Thread.sleep(1000)
                    Delivery[XiaoMing](t, Option(new Timestamp(Calendar.getInstance.getTime.getTime)))
                }
            }

            override def XiaoZhangDelivery: Kleisli[Future, XiaoMingDeliveryT, XiaoZhangDeliveryT] = Kleisli {d =>
                d.endTime match {
                    case Some(t) => Future.successful {
                        Thread.sleep(1000)
                        println(s"[Thread-${Thread.currentThread.getId}] End: $t")
                        Delivery[XiaoZhang](t, Option(new Timestamp(Calendar.getInstance.getTime.getTime)))
                    }
                    case None => Future.failed(new RuntimeException(""))
                }
            }
        }

        /** 使用 */
        import Logistic._
        import scala.concurrent.Await
        import scala.concurrent.duration._

        val start = new Timestamp(Calendar.getInstance.getTime.getTime)
        Await.result(deliveryPackage(start), 10 seconds)
    }
}
