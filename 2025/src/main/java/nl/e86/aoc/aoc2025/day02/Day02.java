package nl.e86.aoc.aoc2025.day02;

import nl.e86.aoc.aoc2025.day02.model.Range;
import nl.e86.aoc.aoc2025.util.InputDataReader;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day02 {
    private static final Pattern RANGE_PATERN = Pattern.compile("([0-9]+-[0-9]+)");
    public static String calculatePart1(String filename) {
        List<String> input = InputDataReader.readInput(filename);
        final AtomicLong sum = new AtomicLong(0);
        List<Range> ranges = Collections.emptyList();
        for (String line : input) {
            Matcher matcher = RANGE_PATERN.matcher(line);
            ranges = matcher.results()
                    .map(MatchResult::group)
                    .map(Range::new)
                    .toList();
        }
        ranges.stream()
                .map(Range::getInvalidIds)
                .flatMap(Collection::stream)
                .forEach(sum::addAndGet);
        return String.valueOf(sum);
    }

    public static void main(String[] args) {
        System.out.println("Part 1: " + calculatePart1("day02.input"));
    }
}