package nl.e86.aoc.aoc2025.day05.model;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

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

    public long countIngredientsConsideredFresh() {
        long amount = 0;
        Set<Range> freshIngredients = new TreeSet<>();
        for (Range range : new TreeSet<>(ranges)) {
            addRange(freshIngredients, range);
        }
        for (Range freshIngredient : freshIngredients) {
            amount += freshIngredient.getSize();
        }
        return amount;
    }

    private static void addRange(Set<Range> freshIngredients, Range range) {
        boolean merged = false;
        for (Range freshIngredient : freshIngredients) {
            if (freshIngredient.merge(range)) {
                merged = true;
                break;
            }
        }
        if (!merged) {
            freshIngredients.add(range);
        }
    }
}
