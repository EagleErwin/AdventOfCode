package nl.e86.aoc.aoc2025.day04.model;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Warehouse {
    private final Set<Position> paperRollLocations = new HashSet<>();

    public Warehouse(List<String> input) {
        int row = 0;
        for (String line : input) {
            for (int col = 0; col < line.length(); ++col) {
                if (line.charAt(col) == '@') {
                    Position roll = new Position(row, col);
                    paperRollLocations.add(roll);
                }
            }
            row++;
        }
    }

    public int getAccessibleRolls(int limitOfAdjacentRolls) {
        return (int) paperRollLocations.stream()
                .filter(n -> this.getNumberOfAdjacentRolls(n) < limitOfAdjacentRolls)
                .count();
    }

    private int getNumberOfAdjacentRolls(Position p) {
        int result = 0;
        // N
        if (paperRollLocations.contains(new Position(p.x, p.y - 1))) {
            result += 1;
        }
        // NE
        if (paperRollLocations.contains(new Position(p.x + 1, p.y - 1))) {
            result += 1;
        }
        // E
        if (paperRollLocations.contains(new Position(p.x + 1, p.y))) {
            result += 1;
        }
        // SE
        if (paperRollLocations.contains(new Position(p.x + 1, p.y + 1))) {
            result += 1;
        }
        // S
        if (paperRollLocations.contains(new Position(p.x, p.y + 1))) {
            result += 1;
        }
        // SW
        if (paperRollLocations.contains(new Position(p.x - 1, p.y + 1))) {
            result += 1;
        }
        // W
        if (paperRollLocations.contains(new Position(p.x - 1, p.y))) {
            result += 1;
        }
        // NW
        if (paperRollLocations.contains(new Position(p.x - 1, p.y - 1))) {
            result += 1;
        }
        return result;
    }
}
