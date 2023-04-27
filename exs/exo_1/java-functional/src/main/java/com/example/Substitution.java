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

    public static double sideEffectWithoutReassignment() {
        // random number 
        return Math.random();
    }

    public static void answerQuestion1() {
        int a, b = 41;

        a = b++;
        // now, a != b
        System.out.println("a = " + a + ", b = " + b);
        System.out.println("a == b: " + (a == b));
    }

    public static void answerQuestion2() {
        // this compiles and run
        System.out.println(true ? 42 : 23/0);

        // this code crashes
        //final int c = 23/0;
        //System.out.println(true ? 42 : c);
        // in Haskell, because of lazy evaluation, this code would not crash
    }
}
