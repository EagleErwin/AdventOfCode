package nl.e86.aoc.aoc2025.day06.model;

import java.util.List;
import java.util.stream.Collectors;

public final class Problem {
    private final Operation operation;
    private final List<Long> values;

    public Problem(List<String> valuesAsString, String operator) {
        this.operation = Operation.fromChar(operator.trim().charAt(0));
        this.values = valuesAsString.stream().map(String::trim).map(Long::parseLong).collect(Collectors.toList());
    }

    public Long getSolution() {
        Long solution;
        switch (operation) {
            case ADD:
                solution = 0L;
                for (Long value : values) {
                    solution += value;
                }
                break;
            case MULTIPLY:
                solution = 1L;
                for (Long value : values) {
                    solution *= value;
                }
                break;
            default:
                solution = 0L;
                break;
        }
        return solution;
    }

    public int getNumberOfValues() {
        return values.size();
    }

    public Long getValueAt(int index) {
        return values.get(index);
    }

    public String getOperator() {
        return operation.toString();
    }
}
