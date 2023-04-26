package com.example;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class SubstitutionTest {
    
    @Test
    void testAdd2NumbersSideEffect() {
        int a = 1;
        int b = 2;

        // side effect: substitution broken
        // the two calls to add2NumbersSideEffect are not equivalent
        assertNotEquals(
            Substitution.add2NumbersSideEffect(a, b), 
            Substitution.add2NumbersSideEffect(a, b)
        );
    }

    @Test
    void testSideEffectWithoutReassignment() {
        // side effect: substitution broken
        // the two calls to sideEffectWithoutReassignment are not equivalent
        assertNotEquals(
            Substitution.sideEffectWithoutReassignment(), 
            Substitution.sideEffectWithoutReassignment()
        );
    }

}
