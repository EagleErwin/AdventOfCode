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
        Assert.assertEquals(0L, Long.parseLong(output));
    }
}
