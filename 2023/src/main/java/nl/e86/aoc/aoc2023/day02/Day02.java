package nl.e86.aoc.aoc2023.day02;

import nl.e86.aoc.aoc2023.util.InputDataReader;

import java.util.ArrayList;
import java.util.List;

public class Day02 {
    private final List<Game> games = new ArrayList<>();

    private Day02() {
        // Prevent instantiation.
    }

    public static void main(String[] args) {
        String inputDataFile = "day02.input";
        System.out.println("Part 1: " + calculatePart1(inputDataFile));
        System.out.println("Part 2: " + calculatePart2(inputDataFile));
    }

    public static int calculatePart1(String filename) {
        Day02 puzzle = createPuzzle(filename);

        int sum = 0;
        for (Game game : puzzle.games) {
            if (game.isValid()) {
                sum += game.getId();
            }
        }
        return sum;
    }

    public static int calculatePart2(String filename) {
        Day02 puzzle = createPuzzle(filename);
        int sum = 0;
        for (Game game : puzzle.games) {
            int gamePower = game.calculatePower();
            sum += gamePower;
    }
        return sum;
    }

    private static Day02 createPuzzle(String filename) {
        Day02 puzzle = new Day02();
        List<String> inputData = InputDataReader.readInput(filename);
        for (String inputLine : inputData) {
            Game currentGame = Game.createFromString(inputLine);
            puzzle.games.add(currentGame);
        }
        return puzzle;
    }
}
