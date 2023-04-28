package com.example;

import java.util.ArrayList;
import java.util.Collection;
import java.util.function.BiFunction;
import java.util.function.Function;

import com.example.MyBiFunction;

public class Map {
    public static <A, R> Iterable<R> map(
        Iterable<A> iterable,
        Function<A, R> f
    ) {
        MyBiFunction<Iterable<A>, Function<A, R>, Iterable<R>> map = new MyBiFunction<Iterable<A>, Function<A, R>, Iterable<R>>() {
            //MyBiFunction: f(a) -> f(b) -> result
            @Override
            public Function<Function<A, R>, Iterable<R>> apply(Iterable<A> a) {
                return new Function<Function<A, R>, Iterable<R>>() {
                    @Override
                    public Iterable<R> apply(Function<A, R> f) {
                        ArrayList<R> listOfR = new ArrayList<R>();
                        for (A element : a) {
                            listOfR.add(f.apply(element));
                        }
                        return listOfR;
                    }
                };
            };
        };
        return map.apply(iterable).apply(f); // similar map.apply(iterable, f)
    }
}
