package nl.e86.aoc.aoc2023.day05;

import org.junit.Assert;
import org.junit.Test;

public class Day05Test {
    @Test
    public void testDay05_1() {
        int output = Day05.calculatePart1("day05.1.input");
        Assert.assertEquals(35, output);
    }
}
