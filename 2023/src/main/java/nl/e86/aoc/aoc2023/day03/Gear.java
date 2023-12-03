package nl.e86.aoc.aoc2023.day03;

import java.util.List;

public class Gear {
    public static final char GEAR_SYMBOL = '*';
    private final int row;
    private final int col;

    public Gear(int row, int col) {
        this.row = row;
        this.col = col;
    }

    public int getRow() {
        return row;
    }

    public int getCol() {
        return col;
    }

    public int calculateRatio(List<String> schematicRepresentation) {
        Tuple parts = new Tuple();

        // 1 2 3
        // 4   5
        // 6 7 8
        Character c1 = getCharAt(schematicRepresentation, row - 1, col - 1);
        Character c2 = getCharAt(schematicRepresentation, row - 1, col);
        Character c3 = getCharAt(schematicRepresentation, row - 1, col + 1);
        Character c4 = getCharAt(schematicRepresentation, row, col - 1);
        Character c5 = getCharAt(schematicRepresentation, row, col + 1);
        Character c6 = getCharAt(schematicRepresentation, row + 1, col - 1);
        Character c7 = getCharAt(schematicRepresentation, row + 1, col);
        Character c8 = getCharAt(schematicRepresentation, row + 1, col + 1);

        if (!isNumber(c2)) {
            if (isNumber(c1)) {
                String partBuffer = String.valueOf(c1);
                int colIndex = col - 2;
                Character previousChar = getCharAt(schematicRepresentation, row - 1, colIndex--);
                while (isNumber(previousChar)) {
                    partBuffer = previousChar + partBuffer;
                    previousChar = getCharAt(schematicRepresentation, row - 1, colIndex--);
                }
                parts.addValue(Integer.parseInt(partBuffer));
            }
            if (isNumber(c3)) {
                String partBuffer = String.valueOf(c3);
                int colIndex = col + 2;
                Character nexChar = getCharAt(schematicRepresentation, row - 1, colIndex++);
                while (isNumber(nexChar)) {
                    partBuffer = partBuffer + nexChar;
                    nexChar = getCharAt(schematicRepresentation, row - 1, colIndex++);
                }
                parts.addValue(Integer.parseInt(partBuffer));
            }
        } else {
            String partBuffer = String.valueOf(c2);
            int colIndex = col - 1;
            Character previousChar = getCharAt(schematicRepresentation, row - 1, colIndex--);
            while (isNumber(previousChar)) {
                partBuffer = previousChar + partBuffer;
                previousChar = getCharAt(schematicRepresentation, row - 1, colIndex--);
            }
            colIndex = col + 1;
            Character nexChar = getCharAt(schematicRepresentation, row - 1, colIndex++);
            while (isNumber(nexChar)) {
                partBuffer = partBuffer + nexChar;
                nexChar = getCharAt(schematicRepresentation, row - 1, colIndex++);
            }
            parts.addValue(Integer.parseInt(partBuffer));
        }

        if (isNumber(c4)) {
            String partBuffer = String.valueOf(c4);
            int colIndex = col - 2;
            Character previousChar = getCharAt(schematicRepresentation, row, colIndex--);
            while (isNumber(previousChar)) {
                partBuffer = previousChar + partBuffer;
                previousChar = getCharAt(schematicRepresentation, row, colIndex--);
            }
            parts.addValue(Integer.parseInt(partBuffer));
        }
        if (isNumber(c5)) {
            String partBuffer = String.valueOf(c5);
            int colIndex = col + 2;
            Character nexChar = getCharAt(schematicRepresentation, row, colIndex++);
            while (isNumber(nexChar)) {
                partBuffer = partBuffer + nexChar;
                nexChar = getCharAt(schematicRepresentation, row, colIndex++);
            }
            parts.addValue(Integer.parseInt(partBuffer));
        }

        if (!isNumber(c7)) {
            if (isNumber(c6)) {
                String partBuffer = String.valueOf(c6);
                int colIndex = col - 2;
                Character previousChar = getCharAt(schematicRepresentation, row + 1, colIndex--);
                while (isNumber(previousChar)) {
                    partBuffer = previousChar + partBuffer;
                    previousChar = getCharAt(schematicRepresentation, row + 1, colIndex--);
                }
                parts.addValue(Integer.parseInt(partBuffer));
            }
            if (isNumber(c8)) {
                String partBuffer = String.valueOf(c8);
                int colIndex = col + 2;
                Character nexChar = getCharAt(schematicRepresentation, row + 1, colIndex++);
                while (isNumber(nexChar)) {
                    partBuffer = partBuffer + nexChar;
                    nexChar = getCharAt(schematicRepresentation, row + 1, colIndex++);
                }
                parts.addValue(Integer.parseInt(partBuffer));
            }
        } else {
            String partBuffer = String.valueOf(c7);
            int colIndex = col - 1;
            Character previousChar = getCharAt(schematicRepresentation, row + 1, colIndex--);
            while (isNumber(previousChar)) {
                partBuffer = previousChar + partBuffer;
                previousChar = getCharAt(schematicRepresentation, row + 1, colIndex--);
            }
            colIndex = col + 1;
            Character nexChar = getCharAt(schematicRepresentation, row + 1, colIndex++);
            while (isNumber(nexChar)) {
                partBuffer = partBuffer + nexChar;
                nexChar = getCharAt(schematicRepresentation, row + 1, colIndex++);
            }
            parts.addValue(Integer.parseInt(partBuffer));
        }

        return parts.getValue();
    }

    private Character getCharAt(List<String> data, int row, int col) {
        try {
            return data.get(row).charAt(col);
        } catch (IndexOutOfBoundsException e) {
            return null;
        }
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

    private static final class Tuple {
        Integer x;
        Integer y;

        void addValue(int val) {
            if (x == null) {
                x = val;
            } else if (y == null) {
                y = val;
            } else {
                System.err.println("Unable to add a number for the third time, then it's not a gear!");
            }
        }

        int getValue() {
            if (x != null && y != null) {
                return x * y;
            } else {
                return -1;
            }
        }
    }
}
