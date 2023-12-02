package nl.e86.aoc.aoc2023.day02;

import org.junit.Assert;
import org.junit.Test;

public class Day02Test {
    @Test
    public void testDay02_1() {
        int output = Day02.calculatePart1("day02.1.input");
        Assert.assertEquals(8, output);
    }

    @Test
    public void testDay02_2() {
        int output = Day02.calculatePart2("day02.1.input");
        Assert.assertEquals(2286, output);
    }

    @Test
    public void testDay02_wrong_answers() {
        int output = Day02.calculatePart2("day02.input");
        Assert.assertTrue(output < 125622);
    }

}
