package com.hibiup.example15;

import cats.data.Kleisli;
import cats.data.NonEmptyList;
import com.hibiup.example15.Example_15_Export_To_Java.InterestService;
import scala.Option;
import scala.math.BigDecimal;
import scala.util.Either;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

public class JavaInterestService<Account> implements InterestService<Account> {
    @Override
    public Either<NonEmptyList<String>, Option<BigDecimal>> computeInterest(Account acount) {
        throw new NotImplementedException();
    }

    // 以下 IDE 警告可以忽略
    @Override
    public Kleisli<Either, Option<BigDecimal>, BigDecimal> computeTax() {
        throw new NotImplementedException();
    }
}