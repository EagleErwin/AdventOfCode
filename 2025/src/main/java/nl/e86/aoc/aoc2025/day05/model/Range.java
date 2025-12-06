package nl.e86.aoc.aoc2025.day05.model;

import java.util.Arrays;
import java.util.List;

public final class Range {
    private final long beginId;
    private final long endId;

    public Range(String input) {
        List<Long> beginEnd = Arrays.stream(input.split("-")).map(Long::parseLong).toList();
        this.beginId = beginEnd.get(0);
        this.endId = beginEnd.get(1);
    }

    public boolean isFresh(long ingredient) {
        return beginId <= ingredient && ingredient <= endId;
    }
}
