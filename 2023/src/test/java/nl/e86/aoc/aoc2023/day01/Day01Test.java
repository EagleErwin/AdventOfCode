package nl.e86.aoc.aoc2023.day01;

import org.junit.Assert;
import org.junit.Test;

public class Day01Test {
    @Test
    public void testDay01_1() {
        String output = Day01.calculatePart1("day01.1.input");
        Assert.assertEquals(142, Integer.parseInt(output));
    }

    @Test
    public void testDay01_2() {
        String output = Day01.calculatePart2("day01.2.input");
        Assert.assertEquals(281, Integer.parseInt(output));
    }
}
