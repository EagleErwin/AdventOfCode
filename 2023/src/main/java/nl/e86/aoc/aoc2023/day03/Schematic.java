package nl.e86.aoc.aoc2023.day03;

import java.util.ArrayList;
import java.util.List;

public class Schematic {
    private final List<String> schematicRepresentation;

    private Schematic(List<String> representation) {
        schematicRepresentation = representation;
    }

    public int getSolutionPt1() {
        int sum = 0;
        for (int i = 0; i < getLength(); ++i) {
            List<Integer> partNumbers = getPartNumbersForRow(i);
            for (Integer partNumber : partNumbers) {
                sum += partNumber;
            }
        }
        return sum;
    }

    private List<Integer> getPartNumbersForRow(int row) {
        List<Integer> result = new ArrayList<>();

        String rowString = schematicRepresentation.get(row);
        int startIndex = -1;
        for (int i = 0; i < rowString.length(); ++i) {
            char currentChar = rowString.charAt(i);
            if (isNumber(currentChar)) {
                if (startIndex == -1) {
                    startIndex = i;
                }
            }
            if (startIndex != -1 && !isNumber(currentChar)) {
                String number = rowString.substring(startIndex, i);
                if (isPartNumber(number, row, startIndex)) {
                    result.add(Integer.parseInt(number));
                }
                startIndex = -1;
            }
        }
        if (startIndex != -1) {
            String number = rowString.substring(startIndex);
            if (isPartNumber(number, row, startIndex)) {
                result.add(Integer.parseInt(number));
            }
        }

        return result;
    }

    /**
     * Returns whether the number starting at (row, column) is a part number.
     */
    private boolean isPartNumber(String number, int row, int column) {
        boolean symbolFound = false;
        int numberLength = number.length();
        String rowString = schematicRepresentation.get(row);

        int startIndex = Math.max(0, column - 1);
        int stopIndex = Math.min(rowString.length() - 1, column + numberLength);

        // We don't care that we are checking our first number if we are in the first column, that's a number and not a symbol.
        char previousChar = rowString.charAt(startIndex);
        symbolFound |= isSymbol(previousChar);

        char nextChar = rowString.charAt(stopIndex);
        symbolFound |= isSymbol(nextChar);

        if (row > 0) {
            String previousRow = schematicRepresentation.get(row -1);
            for (int i = startIndex; i <= stopIndex; ++i) {
                symbolFound |= isSymbol(previousRow.charAt(i));
            }
        }
        if (row < getLength() - 1) {
            String nextRow = schematicRepresentation.get(row + 1);
            for (int i = startIndex; i <= stopIndex; ++i) {
                symbolFound |= isSymbol(nextRow.charAt(i));
            }
        }
        return symbolFound;
    }

    public static Schematic createFromInput(List<String> input) {
        List<String> representation = new ArrayList<>();
        for (String inputLine : input) {
            representation.add(inputLine);
        }
        return new Schematic(representation);
    }

    private static boolean isSymbol(char character) {
        return character != '.' && !isNumber(character);
    }

    private static boolean isNumber(char character) {
        try {
            Integer.parseInt(character + "");
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public int getLength() {
        return schematicRepresentation.size();
    }
}

