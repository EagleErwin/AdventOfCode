package nl.e86.aoc.aoc2023.day04;

import org.junit.Assert;
import org.junit.Test;

public class Day04Test {
    @Test
    public void testDay04_1() {
        int output = Day04.calculatePart1("day04.1.input");
        Assert.assertEquals(13, output);
    }

    @Test
    public void testDay04_2() {
        int output = Day04.calculatePart2("day04.1.input");
        Assert.assertEquals(30, output);
    }
}
