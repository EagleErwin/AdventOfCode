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

    public int getSolutionPt2() {
        int sum = 0;
        for (Gear gear : findGears()) {
            int ratio = gear.calculateRatio(schematicRepresentation);
            sum += ratio;
        }
        return sum;
    }

    private List<Gear> findGears() {
        List<Gear> result = new ArrayList<>();
        for (int rowIndex = 0; rowIndex < getLength(); ++rowIndex) {
            String rowString = schematicRepresentation.get(rowIndex);
            for (int colIndex = 0; colIndex < rowString.length(); ++colIndex) {
                if (rowString.charAt(colIndex) == Gear.GEAR_SYMBOL) {
                    Gear candiateGear = new Gear(rowIndex, colIndex);
                    if (hasExactlyTwoPartNumberAround(candiateGear)) {
                        result.add(candiateGear);
                    }
                }
            }
        }
        return result;
    }

    private boolean hasExactlyTwoPartNumberAround(Gear candidate) {
        int numberOfAdjacentParts = 0;
        int startRowIndex = candidate.getRow() - 1;
        int stopRowIndex = candidate.getRow() + 1;

        int startColIndex = candidate.getCol() - 1;
        int stopColIndex = candidate.getCol() + 1;

        // 1 2 3
        // 4   5
        // 6 7 8
        Character c1 = null;
        Character c2 = null;
        Character c3 = null;
        Character c4 = null;
        Character c5 = null;
        Character c6 = null;
        Character c7 = null;
        Character c8 = null;

        if (startRowIndex >= 0) {
            String rowAbove = schematicRepresentation.get(startRowIndex);
            if (startColIndex >= 0) {
                c1 = rowAbove.charAt(startColIndex);
            }
            c2 = rowAbove.charAt(candidate.getCol());
            if (stopColIndex < rowAbove.length()) {
                c3 = rowAbove.charAt(stopColIndex);
            }
        }
        String currentRow = schematicRepresentation.get(candidate.getRow());
        if (startColIndex >= 0) {
            c4 = currentRow.charAt(startColIndex);
        }
        if (stopColIndex < currentRow.length()) {
            c5 = currentRow.charAt(stopColIndex);
        }
        if (stopRowIndex < getLength()) {
            String rowBelow = schematicRepresentation.get(stopRowIndex);
            if (startColIndex >= 0) {
                c6 = rowBelow.charAt(startColIndex);
            }
            c7 = rowBelow.charAt(candidate.getCol());
            if (stopColIndex < rowBelow.length()) {
                c8 = rowBelow.charAt(stopColIndex);
            }
        }

        if (isNumber(c1)) {
            if (!isNumber(c2) && isNumber(c3)) {
                // N . N
                // ? * ?
                // ? ? ?
                numberOfAdjacentParts = numberOfAdjacentParts + 2;
            } else {
                // N N _  // N . .
                // ? * ?  // ? * ?
                // ? ? ?  // ? ? ?
                numberOfAdjacentParts++;
            }
        } else {
            if (isNumber(c2) || isNumber(c3)) {
                // . . N // . N N // . N .
                numberOfAdjacentParts++;
            }
        }

        if (isNumber(c4)) {
            numberOfAdjacentParts++;
        }
        if (isNumber(c5)) {
            numberOfAdjacentParts++;
        }

        if (isNumber(c6)) {
            if (!isNumber(c7) && isNumber(c8)) {
                // N . N
                numberOfAdjacentParts = numberOfAdjacentParts + 2;
            } else {
                // N N _  // N . .
                numberOfAdjacentParts++;
            }
        } else {
            if (isNumber(c7) || isNumber(c8)) {
                // . . N // . N N // . N .
                numberOfAdjacentParts++;
            }
        }

        return numberOfAdjacentParts == 2;
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

    private static boolean isNumber(Character character) {
        if (character == null) { return false; }
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