package nl.e86.aoc.aoc2023.day02;

import nl.e86.aoc.aoc2023.util.InputDataReader;

import java.util.ArrayList;
import java.util.List;

public class Day02 {
    private static final List<Game> GAMES = new ArrayList<>();
    public static void main(String[] args) {
        String inputDataFile = "day02.input";
        System.out.println("Part 1: " + calculatePart1(inputDataFile));
        System.out.println("Part 2: " + calculatePart2(inputDataFile));
    }

    public static int calculatePart1(String filename) {
        List<String> inputData = InputDataReader.readInput(filename);
        for (String inputLine : inputData) {
            Game currentGame = Game.createFromString(inputLine);
            GAMES.add(currentGame);
        }

        int sum = 0;
        for (Game game : GAMES) {
            if (game.isValid()) {
                sum += game.getId();
            }
        }
        return sum;
    }

    public static int calculatePart2(String filename) {
        List<String> inputData = InputDataReader.readInput(filename);
        for (String inputLine : inputData) {
            Game currentGame = Game.createFromString(inputLine);
            GAMES.add(currentGame);
        }
        return -1;
    }
}
