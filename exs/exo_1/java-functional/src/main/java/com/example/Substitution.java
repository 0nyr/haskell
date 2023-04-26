package com.example;

public class Substitution {
    static int sideEffect = 0;

    public static int getSideEffect() {
        return sideEffect;
    }

    public static int add2NumbersSideEffect(
        int a, int b
    ) {
        sideEffect += 1;
        return a + b + sideEffect;
    }
}
