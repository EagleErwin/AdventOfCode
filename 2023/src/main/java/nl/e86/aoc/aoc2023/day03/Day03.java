package nl.e86.aoc.aoc2023.day03;

import nl.e86.aoc.aoc2023.util.InputDataReader;

import java.util.List;

public class Day03 {
    private final Schematic inputSchematic;
    private Day03(Schematic inputSchematic) {
        this.inputSchematic = inputSchematic;
    }

    public static void main(String[] args) {
        String inputDataFile = "day03.input";
        System.out.println("Part 1: " + calculatePart1(inputDataFile));
        System.out.println("Part 2: " + calculatePart2(inputDataFile));
    }

    public static int calculatePart1(String filename) {
        Day03 puzzle = createPuzzle(filename);

        return puzzle.inputSchematic.getSolutionPt1();
    }

    public static int calculatePart2(String filename) {
        Day03 puzzle = createPuzzle(filename);

        return -1;
    }

    private static Day03 createPuzzle(String filename) {
        List<String> inputData = InputDataReader.readInput(filename);
        Schematic schematic = Schematic.createFromInput(inputData);

        return new Day03(schematic);
    }
}
