package nl.e86.aoc.aoc2025.day03;

import org.junit.Assert;
import org.junit.Test;

public class Day03Test {
    @Test
    public void testDay03_1() {
        String output = Day03.calculatePart1("day03.1.input");
        Assert.assertEquals(357L, Long.parseLong(output));
    }

    @Test
    public void testDay03_2() {
        String output = Day03.calculatePart2("day03.1.input");
        Assert.assertEquals(3121910778619L, Long.parseLong(output));
    }
}
