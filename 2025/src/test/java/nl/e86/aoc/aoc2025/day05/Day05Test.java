package nl.e86.aoc.aoc2025.day05;

import org.junit.Assert;
import org.junit.Test;

public class Day05Test {
    @Test
    public void testDay05_1() {
        String output = Day05.calculatePart1("day05.1.input");
        Assert.assertEquals(3L, Long.parseLong(output));
    }

    @Test
    public void testDay05_2() {
        String output = Day05.calculatePart2("day05.1.input");
        Assert.assertEquals(14L, Long.parseLong(output));
    }

    // 330579068691085 too low

    @Test
    public void testDay05_real_input() {
        String output = Day05.calculatePart2("day05.input");
        System.out.println(output);
        Assert.assertTrue("Result should be larger than 330579068691085", 330579068691085L <  Long.parseLong(output));
        Assert.assertTrue("Result should be larger than 334482544157383", 334482544157383L <  Long.parseLong(output));
    }
}
