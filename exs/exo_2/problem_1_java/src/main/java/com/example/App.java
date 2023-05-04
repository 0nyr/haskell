package com.example;

import java.util.LinkedList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;


public class App
{
    // correction: easier simpler version of Map
    static class MapCorrection<A, R> implements MyBiFunction<Function<A, R>, Iterable<A>, Iterable<R>> {
        @Override
        public Iterable<R> apply(Function<A, R> f, Iterable<A> input) {
            final List<R> result = new LinkedList<>();
            for (A element : input) {
                result.add(f.apply(element));
            }
            return result;
        }

        @Override
        public Function<Iterable<A>, Iterable<R>> apply(Function<A, R> arg0) {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'apply'");
        }
    }


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
