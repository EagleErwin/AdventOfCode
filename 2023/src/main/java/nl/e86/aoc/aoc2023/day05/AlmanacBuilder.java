package nl.e86.aoc.aoc2023.day05;

import java.util.ArrayList;
import java.util.List;

public class AlmanacBuilder {
    final List<Long> seeds = new ArrayList<>();
    final AlmanacMap seedToSoilMap = new AlmanacMap();
    final AlmanacMap soilToFertilizerMap = new AlmanacMap();
    final AlmanacMap fertilizerToWaterMap = new AlmanacMap();
    final AlmanacMap waterToLightMap = new AlmanacMap();
    final AlmanacMap lightToTemperatureMap = new AlmanacMap();
    final AlmanacMap temperatureToHumidityMap = new AlmanacMap();
    final AlmanacMap humidityToLocationMap = new AlmanacMap();

    AlmanacBuilder() {
        // Nothing to do.
    }

    AlmanacBuilder setSeeds(String inputString) {
        this.seeds.clear();
        String seedNumbers = inputString.substring(7);
        for (String seedNumber : seedNumbers.split(" ")) {
            this.seeds.add(Long.parseLong(seedNumber));
        }
        return this;
    }

    AlmanacBuilder setSeedToSoilMap(List<String> inputStrings) {
        for (String inputString : inputStrings) {
            seedToSoilMap.addMapping(inputString);
        }
        return this;
    }

    AlmanacBuilder setSoilToFertilizerMap(List<String> inputStrings) {
        for (String inputString : inputStrings) {
            soilToFertilizerMap.addMapping(inputString);
        }
        return this;
    }

    AlmanacBuilder setFertilizerToWaterMap(List<String> inputStrings) {
        for (String inputString : inputStrings) {
            fertilizerToWaterMap.addMapping(inputString);
        }
        return this;
    }

    AlmanacBuilder setWaterToLightMap(List<String> inputStrings) {
        for (String inputString : inputStrings) {
            waterToLightMap.addMapping(inputString);
        }
        return this;
    }

    AlmanacBuilder setLightToTemperatureMap(List<String> inputStrings) {
        for (String inputString : inputStrings) {
            lightToTemperatureMap.addMapping(inputString);
        }
        return this;
    }

    AlmanacBuilder setTemperatureToHumidityMap(List<String> inputStrings) {
        for (String inputString : inputStrings) {
            temperatureToHumidityMap.addMapping(inputString);
        }
        return this;
    }

    AlmanacBuilder setHumidityToLocationMap(List<String> inputStrings) {
        for (String inputString : inputStrings) {
            humidityToLocationMap.addMapping(inputString);
        }
        return this;
    }
}
