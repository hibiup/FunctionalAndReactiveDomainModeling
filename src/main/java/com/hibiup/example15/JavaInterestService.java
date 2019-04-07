package com.hibiup.example15;

import cats.Applicative;
import cats.arrow.FunctionK;
import cats.data.Kleisli;
import cats.data.NonEmptyList;
import com.hibiup.example15.Example_15_Export_To_Java.InterestService;
import scala.Option;
import scala.Some;
import scala.math.BigDecimal;
import scala.util.Either;
import scala.util.Right;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

public class JavaInterestService<Account> implements InterestService<Account> {
    @Override
    public Either<NonEmptyList<String>, Option<BigDecimal>> computeInterest(Account acount) {
        return new Right(new Some(BigDecimal.valueOf(1.0)));
    }

    /** 以下 Intellij 警告 "attempting to use incompatible return type" 可以忽略
     * */
    @SuppressWarnings("unchecked")
    @Override
    public /*<A>*/ Kleisli<Either/*<NonEmptyList<String>, A>*/, Option<BigDecimal>, BigDecimal> computeTaxK() {
        // new Right(amount.get())
        throw new NotImplementedException();
    }

    /*@Override
    public Kleisli<Either, Account, BigDecimal> op() {
        return super.op().run().apply(?);
    }*/

}
