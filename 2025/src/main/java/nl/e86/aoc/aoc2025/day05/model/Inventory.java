package nl.e86.aoc.aoc2025.day05.model;

import java.util.HashSet;
import java.util.Set;

public final class Inventory {

    private final Set<Range> ranges = new HashSet<>();
    private final Set<Long> ingredients = new HashSet<>();

    public Inventory() {

    }

    public void addRange(Range range) {
        ranges.add(range);
    }

    public void addIngredient(Long ingredient) {
        ingredients.add(ingredient);
    }

    public int countFreshIngredients() {
        int amount = 0;
        for (Long ingredient : ingredients) {
            for (Range range : ranges) {
                if (range.isFresh(ingredient)) {
                    amount += 1;
                    break;
                }
            }
        }
        return amount;
    }
}
