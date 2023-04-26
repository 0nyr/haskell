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
            // WARN: for - operator, here is the order of calls needed
            // ex_list = [1, 2, 3, 4]
            // call stack:
            // DONT DO THIS:    f(1, f(2, f(3, f(4, init))))
            //  (1 - (2 - (3 - (4 - init)))) = -2               (WRONG)
            // DO THIS:         f(f(f(f(init, 4), 3), 2), 1)
            //  (((init - 4) - 3) - 2) - 1 = -10                (CORRECT)
            // OTHER CORRECT:   f(f(f(f(init, 1), 2), 3), 4)
            //  (((init - 1) - 2) - 3) - 4 = -10                (CORRECT)        

            return f.apply(
                reduceRec(f, init, list.subList(0, list.size() - 1)),
                list.get(list.size() - 1) // last element
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
            return f.apply(
                reduceRecGeneric(f, init, list.subList(0, list.size() - 1)), 
                list.get(list.size() - 1)
            );
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
