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
            type LoanApplication

            // 申请贷款函数 applyLoan 输出一个常规类型 LoanApplication
            def applyLoan(name: String, purpose: String, date: Date): LoanApplication  // 申请

            // LoanApplication 将作为审核贷款函数 approve 和 enrich 的输入
            def approve: Kleisli[Option, LoanApplication, LoanApplication]    // 审核
            def enrich: Kleisli[Option, LoanApplication, LoanApplication]     // 补全数据

            // 定义 approve 和 enrich 的 compose
            val op = approve andThen enrich
        }

        /** 实现 */
        object FormAndProcess extends FormAndProcess{
            case class Application(name: String, purpose: String, openDate: Date )
            override type LoanApplication = Application   // 实现类型定义

            // 实现算法
            override def applyLoan(name: String, purpose: String, date: Date): LoanApplication = {
                // TODO: ...
                Application("John","For fun!", date)
            }

            override def approve: Kleisli[Option, LoanApplication, LoanApplication] = Kleisli{ la: LoanApplication =>
                Option{
                    // TODO: ...
                    la
                }
            }

            override def enrich: Kleisli[Option, LoanApplication, LoanApplication] = Kleisli{ la =>
                Option{
                    // TODO: ...
                    la
                }
            }
        }

        /** 使用 */
        import FormAndProcess._

        // 得到 LoanApplication 实例
        val la = applyLoan("John B Rich", "House Building", Calendar.getInstance.getTime)

        // 应用于 compose
        val res = op run la
        println(res)
    }
}
