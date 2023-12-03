package nl.e86.aoc.aoc2023.day03;

import org.junit.Assert;
import org.junit.Test;

public class Day03Test {
    @Test
    public void testDay03_1() {
        int output = Day03.calculatePart1("day03.1.input");
        Assert.assertEquals(4361, output);
    }

    @Test
    public void testDay03_1_alt() {
        int output = Day03.calculatePart1("day03.2.input");
        Assert.assertEquals(467, output);
    }

    @Test
    public void testDay03_1_overlapping_part_number() {
        int output = Day03.calculatePart1("day03.3.input");
        Assert.assertEquals(614, output);
    }

    @Test
    public void testDay03_1_duplicate_part_number() {
        int output = Day03.calculatePart1("day03.4.input");
        Assert.assertEquals(61, output);
    }

    @Test
    public void testDay03_1_number_on_last_column() {
        int output = Day03.calculatePart1("day03.5.input");
        Assert.assertEquals(122, output);
    }

    @Test
    public void testDay03_1_wrong_answers() {
        int output = Day03.calculatePart1("day03.input");
        Assert.assertTrue("The answer " + output + " should be larger than " + 540037, 540037 < output);
        Assert.assertTrue("The answer " + output + " should be larger than " + 540131, 540131 < output);

    }
}
