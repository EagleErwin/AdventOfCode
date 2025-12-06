package nl.e86.aoc.aoc2025.day05.model;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

public final class Range implements Comparable<Range> {
    private long beginId;
    private long endId;

    public Range(String input) {
        List<Long> beginEnd = Arrays.stream(input.split("-")).map(Long::parseLong).toList();
        this.beginId = beginEnd.get(0);
        this.endId = beginEnd.get(1);
    }

    public boolean isFresh(long ingredient) {
        return beginId <= ingredient && ingredient <= endId;
    }

    public Set<Long> getFreshIds() {
        return LongStream.range(beginId, endId + 1).boxed().collect(Collectors.toSet());
    }

    public long getSize() {
        return 1 + (endId - beginId);
    }

    /**
     * Merges the provided range with this range. If a merge is not possible, return false.
     * @param range the range to merge with.
     * @return boolean indicating whether the merge was successful.
     */
    public boolean merge(Range range) {
        if (range.beginId >= this.beginId && range.beginId <= this.endId) {
            // The provided range starts within this range.
            this.endId = Math.max(this.endId, range.endId);
            return true;
        }
        if (range.endId <= this.endId && range.endId >= this.beginId) {
            // The provided range ends within this range.
            this.beginId = Math.min(this.beginId, range.beginId);
            return true;
        }
        return false;
    }

    @Override
    public int compareTo(Range o) {
        int result = Long.compare(beginId, o.beginId);
        if (result == 0) {
            result = Long.compare(endId, o.endId);
        }
        return result;
    }

    @Override
    public String toString() {
        return "[" + beginId + "," + endId + "]";
    }
}
