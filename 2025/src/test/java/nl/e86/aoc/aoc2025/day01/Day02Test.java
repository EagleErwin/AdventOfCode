package nl.e86.aoc.aoc2025.day01;

import nl.e86.aoc.aoc2025.day02.Day02;
import org.junit.Assert;
import org.junit.Test;

public class Day02Test {
    @Test
    public void testDay02_1() {
        String output = Day02.calculatePart1("day02.1.input");
        Assert.assertEquals(1227775554, Integer.parseInt(output));
    }
}
