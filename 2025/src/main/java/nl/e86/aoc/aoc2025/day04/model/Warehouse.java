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

    private Warehouse(Set<Position> paperRollLocations) {
        this.paperRollLocations.addAll(paperRollLocations);
    }

    public int getNumberOfRolls() {
        return this.paperRollLocations.size();
    }

    public int getAccessibleRolls(int limitOfAdjacentRolls) {
        return (int) paperRollLocations.stream()
                .filter(n -> this.getNumberOfAdjacentRolls(n) < limitOfAdjacentRolls)
                .count();
    }

    public int getNumberOfRemovedRolls(int limitOfAdjacentRolls) {
        int result = 0;
        int numberOfRolls = getNumberOfRolls();
        int removedRolls;
        Warehouse step = this;
        do {
            step = step.reduce(limitOfAdjacentRolls);
            removedRolls = numberOfRolls - step.getNumberOfRolls();
            result += removedRolls;
            numberOfRolls = step.getNumberOfRolls();
        } while (removedRolls != 0);
        return result;
    }

    private Warehouse reduce(int limitOfAdjacentRolls) {
        Warehouse result = new Warehouse(paperRollLocations);
        paperRollLocations.stream()
                .filter(n -> this.getNumberOfAdjacentRolls(n) < limitOfAdjacentRolls)
                .forEach(result::removeRoll);
        return result;
    }

    private void removeRoll(Position p) {
        this.paperRollLocations.remove(p);
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
