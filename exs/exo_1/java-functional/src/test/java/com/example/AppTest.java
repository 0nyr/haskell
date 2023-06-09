package com.example;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.List;
import java.util.function.BinaryOperator;

import com.example.App;

class AppTest {

    @Test
    void testReduceWithLoop() {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        BinaryOperator<Integer> sum = Integer::sum;
        int expectedSum = 15;
        assertEquals(expectedSum, App.reduce(sum, 0, numbers));

        List<String> strings = Arrays.asList("a", "b", "c");
        BinaryOperator<String> concat = String::concat;
        String expectedConcat = "abc";
        assertEquals(expectedConcat, App.reduce(concat, "", strings));
    }

    @Test
    void testReduceWithRecursion() {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        BinaryOperator<Integer> sum = Integer::sum;
        int expectedSum = 15;
        assertEquals(expectedSum, App.reduceRec(sum, 0, numbers));

        List<String> strings = Arrays.asList("a", "b", "c");
        BinaryOperator<String> concat = String::concat;
        String expectedConcat = "abc";
        assertEquals(expectedConcat, App.reduceRec(concat, "", strings));
    }

    @Test
    void testReduceWithEmptyCollection() {
        BinaryOperator<Integer> sum = Integer::sum;
        int expectedSum = 0;
        assertEquals(expectedSum, App.reduce(sum, 0, Arrays.asList()));

        BinaryOperator<String> concat = String::concat;
        String expectedConcat = "";
        assertEquals(expectedConcat, App.reduce(concat, "", Arrays.asList()));
    }

    @Test
    void testReduceRecWithEmptyCollection() {
        BinaryOperator<Integer> sum = Integer::sum;
        int expectedSum = 0;
        assertEquals(expectedSum, App.reduceRec(sum, 0, Arrays.asList()));

        BinaryOperator<String> concat = String::concat;
        String expectedConcat = "";
        assertEquals(expectedConcat, App.reduceRec(concat, "", Arrays.asList()));
    }



    @Test
    void testReduceWithLoopGeneric() {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        BinaryOperator<Integer> sum = Integer::sum;
        int expectedSum = 15;
        assertEquals(expectedSum, App.reduceGeneric(sum, 0, numbers));

        List<String> strings = Arrays.asList("a", "b", "c");
        BinaryOperator<String> concat = String::concat;
        String expectedConcat = "abc";
        assertEquals(expectedConcat, App.reduceGeneric(concat, "", strings));
    }

    @Test
    void testReduceWithRecursionGeneric() {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        BinaryOperator<Integer> sum = Integer::sum;
        int expectedSum = 15;
        assertEquals(expectedSum, App.reduceRecGeneric(sum, 0, numbers));

        List<String> strings = Arrays.asList("a", "b", "c");
        BinaryOperator<String> concat = String::concat;
        String expectedConcat = "abc";
        assertEquals(expectedConcat, App.reduceRecGeneric(concat, "", strings));
    }

    @Test
    void testReduceWithEmptyCollectionGeneric() {
        BinaryOperator<Integer> sum = Integer::sum;
        int expectedSum = 0;
        assertEquals(expectedSum, App.reduceGeneric(sum, 0, Arrays.asList()));

        BinaryOperator<String> concat = String::concat;
        String expectedConcat = "";
        assertEquals(expectedConcat, App.reduceGeneric(concat, "", Arrays.asList()));
    }

    @Test
    void testReduceRecWithEmptyCollectionGeneric() {
        BinaryOperator<Integer> sum = Integer::sum;
        int expectedSum = 0;
        assertEquals(expectedSum, App.reduceRecGeneric(sum, 0, Arrays.asList()));

        BinaryOperator<String> concat = String::concat;
        String expectedConcat = "";
        assertEquals(expectedConcat, App.reduceRecGeneric(concat, "", Arrays.asList()));
    }

    @Test
    void testNegativeLambda() {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4);

        int totalAge0 = App.reduceRecGeneric(
            (acc, num) -> acc - num, // BiFunction that takes an accumulator and a person and returns the new accumulator
            0,
            numbers
        );

        int totalAge1 = App.reduceRec(
            (acc, num) -> acc - num, // BiFunction that takes an accumulator and a person and returns the new accumulator
            0,
            numbers
        );

        int totalAge2 = App.reduce(
            (acc, num) -> acc - num, // BiFunction that takes an accumulator and a person and returns the new accumulator
            0,
            numbers
        );

        int totalAge3 = App.reduceGeneric(
            (acc, num) -> acc - num, // BiFunction that takes an accumulator and a person and returns the new accumulator
            0,
            numbers
        );

        assertEquals(-10, totalAge0);
        assertEquals(-10, totalAge1);
        assertEquals(-10, totalAge2);
        assertEquals(-10, totalAge3);
    }

}