package com.example;

import java.util.function.BinaryOperator;
import java.util.function.BiFunction;
import java.util.Collection;
import java.util.List;
import java.util.Arrays;

class Person {
    private String name;
    private int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String getName() {
        return name;
    }

    public int getAge() {
        return age;
    }
}


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

    public static <A, B> B reduceGeneric(
            BiFunction<B, A, B> f,
            B init,
            Collection<A> collection
    ) {
        // perform reduction with loop
        B accumulator = init;
        for (A element : collection) {
            accumulator = f.apply(accumulator, element);
        }
        return accumulator;
    }

    public static <A, B> B reduceRecGeneric(
            BiFunction<B, A, B> f,
            B init,
            List<A> list
    ) {
        // perform reduction with recursion
        if (list.isEmpty()) {
            return init;
        } else {
            if (list.isEmpty()) {
                return init;
            } else {
                B newInit = f.apply(init, list.get(0));
                return reduceRecGeneric(f, newInit, list.subList(1, list.size()));
            }
        }
    }


    public static void main(String[] args) {
        List<Person> people = Arrays.asList(
            new Person("Alice", 30),
            new Person("Bob", 25),
            new Person("Charlie", 45),
            new Person("David", 37)
        );

        int totalAge = reduceRecGeneric(
            (acc, person) -> acc + person.getAge(), // BiFunction that takes an accumulator and a person and returns the new accumulator
            0,
            people
        );

        System.out.println("Total age: " + totalAge); // Output: Total age: 137
    }
}
