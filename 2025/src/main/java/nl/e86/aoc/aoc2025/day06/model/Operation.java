package nl.e86.aoc.aoc2025.day06.model;

public enum Operation {
    ADD,
    MULTIPLY;

    public static Operation fromChar(char c) {
        if (c == '+') {
            return ADD;
        }
        if (c == '*') {
            return MULTIPLY;
        }
        return null;
    }
}
