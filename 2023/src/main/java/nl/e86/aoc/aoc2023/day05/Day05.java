package nl.e86.aoc.aoc2023.day05;

import nl.e86.aoc.aoc2023.util.InputDataReader;

import java.util.List;

public class Day05 {
    private final Almanac almanac;

    private Day05(Almanac almanac) {
        this.almanac = almanac;
    }
    public static void main(String[] args) {
        String inputDataFile = "day05.input";
        System.out.println("Part 1: " + calculatePart1(inputDataFile));
        System.out.println("Part 2: " + calculatePart2(inputDataFile));
    }

    public static long calculatePart1(String filename) {
        Day05 puzzle = createPuzzle(filename);

        return puzzle.almanac.getSolutionPt1();
    }

    public static long calculatePart2(String filename) {
        Day05 puzzle = createPuzzle(filename);

        return puzzle.almanac.getSolutionPt2();
    }

    private static Day05 createPuzzle(String filename) {
        List<String> inputData = InputDataReader.readInput(filename);
        Almanac almanac = Almanac.createFromInput(inputData);

        return new Day05(almanac);
    }
}
