package com.example;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import com.example.App;

/**
 * Unit test for simple App.
 */
public class AppTest {

    @Test
    void testAdd3toNumber() {
        int a = 2;
        int expected = 5;
        assertEquals(expected, App.add3toNumber(a));
        assertEquals(expected, App.add3toNumber2(a));
    }
}
