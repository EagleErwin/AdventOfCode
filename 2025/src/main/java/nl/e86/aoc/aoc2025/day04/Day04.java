package nl.e86.aoc.aoc2025.day04;

import nl.e86.aoc.aoc2025.day04.model.Warehouse;
import nl.e86.aoc.aoc2025.util.InputDataReader;

import java.util.List;

public class Day04 {
    public static String calculatePart1(String filename) {
        List<String> input = InputDataReader.readInput(filename);
        Warehouse warehouse = new Warehouse(input);
        int sum = warehouse.getAccessibleRolls(4);
        return String.valueOf(sum);
    }

    public static String calculatePart2(String filename) {
        List<String> input = InputDataReader.readInput(filename);
        Warehouse warehouse = new Warehouse(input);
        int sum = warehouse.getNumberOfRemovedRolls(4);
        return String.valueOf(sum);
    }

    public static void main(String[] args) {
        System.out.println("Part 1: " + calculatePart1("day04.input"));
        System.out.println("Part 2: " + calculatePart2("day04.input"));
    }
}