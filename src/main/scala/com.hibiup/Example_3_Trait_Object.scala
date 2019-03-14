package com.hibiup

object Example_3_Trait_Object extends App{

    sealed trait TransactionType
    final case object InterestComputation extends TransactionType
    final case object Dividend extends TransactionType

    trait TaxCalculationTable {
        type T <: TransactionType
        val transactionType: T
    }

    object InterestTaxCalculationTable extends TaxCalculationTable {
        type T = InterestComputation.type
        val transactionType = InterestComputation
    }

    println(InterestTaxCalculationTable.transactionType)
}
