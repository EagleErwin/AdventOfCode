package nl.e86.aoc.aoc2025.day06;

import org.junit.Assert;
import org.junit.Test;

public class Day06Test {
    @Test
    public void testDay06_1() {
        String output = Day06.calculatePart1("day06.1.input");
        Assert.assertEquals(4277556L, Long.parseLong(output));
    }

    @Test
    public void testDay06_real_input() {
        String output = Day06.calculatePart1("day06.input");
        Assert.assertTrue("Result should be larger than 3763883613456", 3763883613456L <  Long.parseLong(output));
        Assert.assertEquals(3785892992137L, Long.parseLong(output));
    }

    @Test
    public void testDay06_2() {
        String output = Day06.calculatePart2("day06.1.input");
        Assert.assertEquals(3263827L, Long.parseLong(output));
    }
}
