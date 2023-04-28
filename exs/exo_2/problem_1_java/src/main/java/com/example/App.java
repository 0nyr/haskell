package com.example;

import java.util.function.BiFunction;
import java.util.function.Function;


public class App
{
    public static int add3toNumber(int a) {
        Function<Integer, Integer> f = x -> x + 3;
        return f.apply(a);
    }

    public static int add3toNumber2(int a) {
        BiFunction<Integer, Integer, Integer> f = (x, y) -> x + y;
        return f.apply(a, 3);
    }

    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
    }
}
