package com.example;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import com.example.MyBiFunction;

public class MyBiFunctionTest {
    @Test
    void testChainedApply() {
        // take a, return function b -> a + b
        MyBiFunction<Integer, Integer, Integer> add = a -> (b -> a + b);

        int a = 2;
        int b = 3;
        int expectedAdd = 5;
        int expectedMult = 6;
        int expectedAddThenMult = 30;
        int expectedMultThenAdd = 11;

        assertEquals(expectedAdd, add.apply(a, b));
        assertEquals(expectedAdd, add.apply(a).apply(b));
        assertEquals(
            add.apply(a, b),
            // call stack:
            //      add.apply(a).apply(b)
            //      (b -> a + b).apply(b)
            //      b -> a + b
            add.apply(a).apply(b)
        );
    }
}
