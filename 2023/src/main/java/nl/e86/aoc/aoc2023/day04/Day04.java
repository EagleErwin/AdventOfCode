package nl.e86.aoc.aoc2023.day04;

import nl.e86.aoc.aoc2023.util.InputDataReader;

import java.util.List;

public class Day04 {
    private final Pile pileOfScratchCards;

    private Day04(Pile pile) {
        this.pileOfScratchCards = pile;
    }
    public static void main(String[] args) {
        String inputDataFile = "day04.input";
        System.out.println("Part 1: " + calculatePart1(inputDataFile));
        System.out.println("Part 2: " + calculatePart2(inputDataFile));
    }

    public static int calculatePart1(String filename) {
        Day04 puzzle = createPuzzle(filename);

        return puzzle.pileOfScratchCards.getSolutionPt1();
    }

    public static int calculatePart2(String filename) {
        Day04 puzzle = createPuzzle(filename);

        return puzzle.pileOfScratchCards.getSolutionPt2();
    }

    private static Day04 createPuzzle(String filename) {
        List<String> inputData = InputDataReader.readInput(filename);
        Pile pile = Pile.createFromInput(inputData);

        return new Day04(pile);
    }
}
