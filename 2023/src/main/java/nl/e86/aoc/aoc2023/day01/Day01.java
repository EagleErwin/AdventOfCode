package nl.e86.aoc.aoc2023.day01;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;

public class Day01 {
    private enum SpelledNumber {
        ONE,
        TWO,
        THREE,
        FOUR,
        FIVE,
        SIX,
        SEVEN,
        EIGHT,
        NINE;

        public String toString() {
            return name().toLowerCase();
        }

        public int value() {
            return this.ordinal() + 1;
        }
    }

    public static String calculatePart1(String filename) {
        List<String> input = readInput(filename);
        int sum = 0;
        for (String line : input) {
            int lineCalibrationValue = getLineCalibrationValue(line);
            sum += lineCalibrationValue;
        }
        return String.valueOf(sum);
    }

    public static String calculatePart2(String filename) {
        List<String> input = readInput(filename);
        int sum = 0;
        for (String line : input) {
            int lineCalibrationValue = getLineCalibrationValueWithSpelledNumbers(line);
            sum += lineCalibrationValue;
        }
        return String.valueOf(sum);
    }

    private static int getLineCalibrationValue(String line) {
        int result;
        Integer firstNumber = null;
        for (char c : line.toCharArray()) {
            if (firstNumber != null) {
                break;
            }
            try {
                firstNumber = Integer.parseInt(String.valueOf(c));
            } catch (NumberFormatException e) {
                // Continue
            }
        }

        Integer lastNumber = null;
        for (int i = line.length() - 1; i >= 0; --i) {
            char c = line.charAt(i);
            if (lastNumber != null) {
                break;
            }
            try {
                lastNumber = Integer.parseInt(String.valueOf(c));
            } catch (NumberFormatException e) {
                // Continue
            }
        }

        if (firstNumber != null & lastNumber != null) {
            result = Integer.parseInt(firstNumber + lastNumber.toString());
        } else {
            System.err.printf("Unable to find first (%s) or last (%s) number.%n", firstNumber, lastNumber);
            result = 0;
        }

        return result;
    }

    private static int getLineCalibrationValueWithSpelledNumbers(String line) {
        int result;
        int firstNumber = getFirstNumber(line);
        int lastNumber = getLastNumber(line);

        result = Integer.parseInt(String.valueOf(firstNumber) + lastNumber);

        return result;
    }

    private static int getFirstNumber(String line) {
        int firstValue = 10;
        int spelledIndex = line.length();
        for (SpelledNumber spelledNumber : SpelledNumber.values()) {
            int index = line.indexOf(spelledNumber.toString());
            if (index != -1 && index < spelledIndex) {
                spelledIndex = index;
                firstValue = spelledNumber.value();
            }
        }

        int numberIndex = IntStream.range(1, 10)
                .map(i -> line.indexOf(String.valueOf(i)))
                .filter(i -> i != -1)
                .sorted()
                .findFirst()
                .orElse(line.length());

        if (numberIndex < spelledIndex) {
            firstValue = Integer.parseInt("" + line.charAt(numberIndex));
        }
        return firstValue;
    }

    private static int getLastNumber(String line) {
        int lastValue = 10;
        int spelledIndex = -1;
        for (SpelledNumber spelledNumber : SpelledNumber.values()) {
            int index = line.lastIndexOf(spelledNumber.toString());
            if (index != -1 && index > spelledIndex) {
                spelledIndex = index;
                lastValue = spelledNumber.value();
            }
        }

        int numberIndex = IntStream.range(1, 10)
                .map(i -> line.lastIndexOf(String.valueOf(i)))
                .filter(i -> i != -1)
                .boxed()
                .max(java.util.Comparator.naturalOrder())
                .orElse(-1);

        if (numberIndex > spelledIndex) {
            lastValue = Integer.parseInt("" + line.charAt(numberIndex));
        }
        return lastValue;
    }

    private static List<String> readInput(String filename) {
        List<String> input = Collections.emptyList();
        URL resource = Day01.class.getClassLoader().getResource(filename);

        if (resource == null) {
            System.err.println("Unable to open resource " + filename);
        } else {
            try (InputStream iStream = resource.openStream();
                 BufferedReader reader = new BufferedReader(new InputStreamReader(iStream))
            ) {
                input = getInputString(reader);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

        }

        return input;
    }

    public static void main(String[] args) {
        System.out.println("Part 1: " + calculatePart1("day01.input"));

        System.out.println("Part 2: " + calculatePart2("day01.input"));
    }

    private static List<String> getInputString(BufferedReader reader) {
        List<String> output = new ArrayList<>();
        try {
            while (reader.ready()) {
                output.add(reader.readLine());
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return output;
    }

}