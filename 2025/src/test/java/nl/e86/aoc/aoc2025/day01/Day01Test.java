package nl.e86.aoc.aoc2025.day01;

import org.junit.Assert;
import org.junit.Test;

public class Day01Test {
    @Test
    public void testDay01_1() {
        String output = Day01.calculatePart1("day01.1.input");
        Assert.assertEquals(3, Integer.parseInt(output));
    }

    @Test
    public void testDay01_2() {
        String output = Day01.calculatePart2("day01.1.input");
        Assert.assertEquals(6, Integer.parseInt(output));
    }
}
