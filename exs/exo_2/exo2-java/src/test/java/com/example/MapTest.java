package com.example;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;

import com.example.Map;

public class MapTest {
    @Test
    void testMap() {
        ArrayList<Integer> list = new ArrayList<Integer>();
        list.add(1);
        list.add(2);
        list.add(3);
        list.add(4);

        ArrayList<Integer> expectedSquares = new ArrayList<Integer>();
        expectedSquares.add(1);
        expectedSquares.add(4);
        expectedSquares.add(9);
        expectedSquares.add(16);

        Iterable<Integer> computeSquares = Map.map(
            list,
            (Integer a) -> a * a
        );
        assertEquals(expectedSquares, computeSquares);

    }
}
