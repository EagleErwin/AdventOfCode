package nl.e86.aoc.aoc2025.day01;

import nl.e86.aoc.aoc2025.util.InputDataReader;

import java.util.List;

public class Day01 {
    public static String calculatePart1(String filename) {
        List<String> input = InputDataReader.readInput(filename);
        int sum = 0;
        int dialPosition = 50;
        for (String line : input) {
            char direction = line.charAt(0);
            int amount = Integer.parseInt(line.substring(1));
            if (direction == 'L') {
                amount *= -1;
            }
            dialPosition += amount;

            while (dialPosition < 0) {
                dialPosition += 100;
            }
            while (dialPosition > 99) {
                dialPosition -= 100;
            }
            if (dialPosition == 0) {
                sum += 1;
            }
        }
        return String.valueOf(sum);
    }

    public static String calculatePart2(String filename) {
        List<String> input = InputDataReader.readInput(filename);
        int sum = 0;
        int dialPosition = 50;
        for (String line : input) {
            char direction = line.charAt(0);
            int amount = Integer.parseInt(line.substring(1));
            if (direction == 'L') {
                amount *= -1;
            }

            Tuple rotateResult = rotate(dialPosition, amount);
            dialPosition = rotateResult.a;
            sum += rotateResult.b;
        }
        return String.valueOf(sum);
    }

    private static Tuple rotate(int position, int amount) {
        int newPosition = position;
        int passes = 0;
        while (amount < 0) {
            newPosition -= 1;
            if (newPosition == 100) {
                newPosition = 0;
            }
            if (newPosition == -1) {
                newPosition = 99;
            }
            if (newPosition == 0) {
                passes += 1;
            }
            amount += 1;
        }
        while (amount > 0) {
            newPosition += 1;
            if (newPosition == 100) {
                newPosition = 0;
            }
            if (newPosition == -1) {
                newPosition = 99;
            }
            if (newPosition == 0) {
                passes += 1;
            }
            amount -= 1;
        }

        return new Tuple(newPosition, passes);
    }

    public static void main(String[] args) {
        System.out.println("Part 1: " + calculatePart1("day01.input"));

        System.out.println("Part 2: " + calculatePart2("day01.input"));
    }

    private static final class Tuple {
        private int a;
        private int b;

        Tuple(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }
}