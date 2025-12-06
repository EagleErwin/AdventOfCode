package nl.e86.aoc.aoc2025.day06;

import nl.e86.aoc.aoc2025.day06.model.Worksheet;
import nl.e86.aoc.aoc2025.util.InputDataReader;

import java.util.List;

public class Day06 {
    public static String calculatePart1(String filename) {
        List<String> input = InputDataReader.readInput(filename);
        Worksheet worksheet = new Worksheet(input);
        Long sum = worksheet.getGrandTotal();
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
        System.out.println("Part 1: " + calculatePart1("day06.input"));
        System.out.println("Part 2: " + calculatePart2("day06.input"));
    }
}