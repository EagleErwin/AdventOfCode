package nl.e86.aoc.aoc2025.day06.model;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public final class Problem {
    private final Operation operation;
    private final List<Long> values;

    public Problem(List<String> valuesAsString, String operator) {
       this(valuesAsString, operator, false);
    }

    public Problem(List<String> valuesAsString, String operator, boolean leftToRight) {
        this.operation = Operation.fromChar(operator.trim().charAt(0));
        if (leftToRight) {
            int numberLength = valuesAsString.get(valuesAsString.size() - 1).length();
            List<StringBuilder> transformedInput = getInitialList(numberLength);
            for (int i = numberLength - 1; i >= 0; --i) {
                for (int j = 0; j < valuesAsString.size(); ++j) {
                    transformedInput.get(i).append(valuesAsString.get(j).charAt(i));
                }
            }
            this.values = transformedInput.stream()
                    .map(StringBuilder::toString)
                    .map(String::trim)
                    .map(Long::parseLong)
                    .collect(Collectors.toList());
        } else {
            this.values = valuesAsString.stream().map(String::trim).map(Long::parseLong).collect(Collectors.toList());
        }
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

    private static List<StringBuilder> getInitialList(int size) {
        List<StringBuilder> result = new ArrayList<>(size);
        for (int i = 0; i < size; ++i) {
            result.add(new StringBuilder());
        }
        return result;
    }
}
