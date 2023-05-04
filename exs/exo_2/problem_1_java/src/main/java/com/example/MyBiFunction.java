package com.example;

import java.util.function.BiFunction;
import java.util.function.Function;

// A is type of imput of top level function
// Function<B,R> is the return type of top level function
//      B is type of input of second level function
//      R is type of return of second level function
interface MyBiFunction<A,B, R> extends Function<A, Function<B,R>> {

    // already DEFINED (abstract) by Function<A, Function<B,R>>
    // default Function<B,R> apply(A a) {
    //     return apply(a);
    // }
    Function <B,R> apply(A a); // user needs to define the unary function

    default R apply(A a, B b) {
        return this.apply(a).apply(b);
    }

}