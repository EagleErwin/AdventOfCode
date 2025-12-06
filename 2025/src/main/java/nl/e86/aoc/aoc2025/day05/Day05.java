package nl.e86.aoc.aoc2025.day05;

import nl.e86.aoc.aoc2025.day05.model.Inventory;
import nl.e86.aoc.aoc2025.day05.model.Range;
import nl.e86.aoc.aoc2025.util.InputDataReader;

import java.util.List;

public class Day05 {
    public static String calculatePart1(String filename) {
        List<String> input = InputDataReader.readInput(filename);
        int sum = 0;
        boolean parseRanges = true;
        Inventory inventory = new Inventory();
        for (String line : input) {
            if (line.isEmpty()) {
                parseRanges = false;
                continue;
            }
            if (parseRanges) {
                inventory.addRange(new Range(line));
            } else {
                inventory.addIngredient(Long.parseLong(line));
            }
        }
        sum = inventory.countFreshIngredients();
        return String.valueOf(sum);
    }

    public static String calculatePart2(String filename) {
        List<String> input = InputDataReader.readInput(filename);
        int sum = 0;
        for (String line : input) {
            // nop
        }
        return String.valueOf(sum);
    }

    public static void main(String[] args) {
        System.out.println("Part 1: " + calculatePart1("day05.input"));
        System.out.println("Part 2: " + calculatePart2("day05.input"));
    }
}