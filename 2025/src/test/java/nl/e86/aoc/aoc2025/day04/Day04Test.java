package nl.e86.aoc.aoc2025.day04;

import org.junit.Assert;
import org.junit.Test;

public class Day04Test {
    @Test
    public void testDay04_1() {
        String output = Day04.calculatePart1("day04.1.input");
        Assert.assertEquals(13L, Long.parseLong(output));
    }

    @Test
    public void testDay04_real_input() {
        String output = Day04.calculatePart1("day04.input");
        Assert.assertTrue("Answer should be larger than 1390", Long.parseLong(output) > 1390);

        Assert.assertFalse("Answer should not be larger than the total number of paper rolls", Long.parseLong(output) > 11956);
    }

    @Test
    public void testDay04_2() {
        String output = Day04.calculatePart2("day04.1.input");
        Assert.assertEquals(43L, Long.parseLong(output));
    }
}
