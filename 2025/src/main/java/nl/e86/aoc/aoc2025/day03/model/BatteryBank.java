package nl.e86.aoc.aoc2025.day03.model;

import java.util.List;
import java.util.stream.Collectors;

public class BatteryBank {
    private final List<Short> batteries;

    public BatteryBank(String input) {
        this.batteries = input.chars()
                .mapToObj(i->(char)i)
                .map(String::valueOf)
                .map(Short::parseShort)
                .collect(Collectors.toList());
    }

    public int getMaxJoltage() {
        short maxValue = 0;
        short bestPosition = 0;
        // Take the highest number from all the batteries but the last.
        for (short i = 0; i < batteries.size() - 1; ++i) {
            short currentValue = batteries.get(i);
            if (currentValue > maxValue) {
                maxValue = currentValue;
                bestPosition = i;
            }
        }

        short bestSecond = 0;
        for (int i = bestPosition + 1; i < batteries.size(); ++i) {
            short currentValue = batteries.get(i);
            if (currentValue > bestSecond) {
                bestSecond = currentValue;
            }
        }
        return maxValue * 10 + bestSecond;
    }
}
