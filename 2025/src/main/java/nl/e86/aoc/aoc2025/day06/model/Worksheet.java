package nl.e86.aoc.aoc2025.day06.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class Worksheet {
    List<Problem> problems = new ArrayList<>();

    public Worksheet(List<String> input) {
        Set<Integer> separators = findSeparators(input);
        int numRows = input.size();
        int beginIndex = 0;
        for (Integer separator : separators) {
            List<String> numbers = new ArrayList<>();
            for (int i = 0; i < numRows - 1; ++i) {
                numbers.add(input.get(i).substring(beginIndex, separator));
            }
            String operator = input.get(numRows - 1).substring(beginIndex, separator);
            this.problems.add(new Problem(numbers, operator));
            beginIndex = separator + 1;
        }
        // We miss the last one.
        List<String> numbers = new ArrayList<>();
        for (int i = 0; i < numRows - 1; ++i) {
            numbers.add(input.get(i).substring(beginIndex).trim());
        }
        String operator = input.get(numRows - 1).substring(beginIndex).trim();
        this.problems.add(new Problem(numbers, operator));
    }

    private static Set<Integer> findSeparators(List<String> input) {
        Set<Integer> separators = new TreeSet<>();
        int lineLength = input.get(0).length();
        int numRows = input.size();
        String firstLine = input.get(0);
        for (int i = 0; i < lineLength; ++i) {
            if (firstLine.charAt(i) == ' ') {
                boolean allSpaces = true;
                for (int j = 0; j < numRows; ++j) {
                    if (input.get(j).charAt(i) != ' ') {
                        allSpaces = false;
                        break;
                    }
                }
                if (allSpaces) {
                    separators.add(i);
                }
            }
        }
        return separators;
    }

    public Long getGrandTotal() {
        Long result = 0L;
        for (Problem problem : problems) {
            result += problem.getSolution();
        }

        return result;
    }
}
