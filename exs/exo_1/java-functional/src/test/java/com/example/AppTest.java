package exs.exo_1;

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
}