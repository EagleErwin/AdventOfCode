package nl.e86.aoc.aoc2023.day05;

import java.util.ArrayList;
import java.util.List;

public class AlmanacMap {
    private final List<MapMapping> mappings = new ArrayList<>();

    public void addMapping(String numbers) {
        MapMapping newMapping = new MapMapping(numbers.split(" "));
        mappings.add(newMapping);
    }

    public Long getDestination(Long source) {
        Long destination = null;
        for (MapMapping mapping : mappings) {
            if (mapping.source <= source && (mapping.source + mapping.length - 1) >= source) {
                destination = source - (mapping.source - mapping.destination);
                break;
            }
        }
        if (destination == null) {
            destination = source;
        }
        return destination;
    }

    private final class MapMapping {
        private final Long destination;
        private final Long source;
        private final Long length;

        MapMapping(String[] numbers) {
            this.destination = Long.parseLong(numbers[0]);
            this.source = Long.parseLong(numbers[1]);
            this.length = Long.parseLong(numbers[2]);
        }
    }
}
