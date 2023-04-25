package com.example;

import java.util.function.BinaryOperator;


public class App 
{
    public static <A> A reduce(
        BinaryOperator<A> f, 
        A init, 
        java.util.Collection<A> collection
    ) {
        // perform reduction with loop
        A accumulator = init;
        for (A element : collection) {
            accumulator = f.apply(accumulator, element);
        }
        return accumulator;
    }

    public static <A> A reduceRec(
        BinaryOperator<A> f, 
        A init, 
        java.util.List<A> list
    ) {
        // perform reduction with recursion
        if (list.isEmpty()) {
            return init;
        } else {
            return f.apply(
                list.get(0), 
                reduceRec(f, init, list.subList(1, list.size()))
            );
        }

    }

    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}
