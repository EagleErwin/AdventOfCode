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

    public long getMaxJoltage(short numberOfBatteries) {
        short batteryPosition = 0;
        long result = 0;
        for (short i = numberOfBatteries; i > 0; --i) {
            Battery bestBattery = getBestBattery(i, batteryPosition);
            result += bestBattery.joltage * Math.pow(10, (i-1));
            batteryPosition = (short)(bestBattery.position + 1);
        }
        return result;
    }

    private Battery getBestBattery(short amount, short startValue) {
        short maxValue = 0;
        short bestPosition = 0;
        for (short i = startValue; i < batteries.size() - (amount - 1); ++i) {
            short currentValue = batteries.get(i);
            if (currentValue > maxValue) {
                maxValue = currentValue;
                bestPosition = i;
            }
        }
        return new Battery(bestPosition, maxValue);
    }

    private final class Battery {
        private final short position;
        private final short joltage;

        private Battery(short position, short joltage) {
            this.position = position;
            this.joltage = joltage;
        }
    }
}
