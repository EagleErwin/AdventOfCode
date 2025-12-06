package nl.e86.aoc.aoc2025.day02.model;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

public class Range {
    private final long begin;
    private final long end;

    public Range(String input) {
        List<Long> beginEnd = Arrays.stream(input.split("-")).map(Long::parseLong).toList();
        this.begin = beginEnd.get(0);
        this.end = beginEnd.get(1);
    }

    public Range(long begin, long end) {
        this.begin = begin;
        this.end = end;
    }

    public Set<Long> getInvalidIds() {
        return LongStream.range(begin, end + 1)
                .boxed()
                .filter(id -> !Range.isValid(id))
                .collect(Collectors.toSet());
    }

    public Set<Long> getInvalidIdsExtended() {
        return LongStream.range(begin, end + 1)
                .boxed()
                .filter(id -> !Range.isValidExtended(id))
                .collect(Collectors.toSet());
    }

    private static boolean isValid(long id) {
        String idAsString = String.valueOf(id);
        if (idAsString.length() % 2 == 0) {
            String firstHalf = idAsString.substring(0, idAsString.length() / 2);
            return !(idAsString.equals(firstHalf + firstHalf));
        }

        return true;
    }

    private static boolean isValidExtended(long id) {
        String idAsString = String.valueOf(id);

        for (int i = 1; i <= (idAsString.length() / 2); ++i) {
            String subStr = idAsString.substring(0, i);
            Pattern matchPattern = Pattern.compile("^(" + subStr + ")+$");
            if (matchPattern.matcher(idAsString).matches()) {
                return false;
            }
        }

        return true;
    }

    @Override
    public String toString() {
        return begin + "-" + end;
    }
}
