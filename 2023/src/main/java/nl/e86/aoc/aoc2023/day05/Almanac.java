package nl.e86.aoc.aoc2023.day05;

import java.util.ArrayList;
import java.util.List;

public class Almanac {
    private static final int SEED_PHASE = 0;
    private static final int SEED_TO_SOIL_PHASE = 1;
    private static final int SOIL_TO_FERTILIZER_PHASE = 2;
    private static final int FERTILIZER_TO_WATER_PHASE = 3;
    private static final int WATER_TO_LIGHT_PHASE = 4;
    private static final int LIGHT_TO_TEMPERATURE_PHASE = 5;
    private static final int TEMPERATURE_TO_HUMIDITY_PHASE = 6;
    private static final int HUMIDITY_TO_LOCATION_PHASE = 7;


    private final List<Long> seeds = new ArrayList<>();
    private final AlmanacMap seedToSoilMap;
    private final AlmanacMap soilToFertilizerMap;
    private final AlmanacMap fertilizerToWaterMap;
    private final AlmanacMap waterToLightMap;
    private final AlmanacMap lightToTemperatureMap;
    private final AlmanacMap temperatureToHumidityMap;
    private final AlmanacMap humidityToLocationMap;

    private Almanac(AlmanacBuilder builder) {
        this.seeds.addAll(builder.seeds);
        this.seedToSoilMap = builder.seedToSoilMap;
        this.soilToFertilizerMap = builder.soilToFertilizerMap;
        this.fertilizerToWaterMap = builder.fertilizerToWaterMap;
        this.waterToLightMap = builder.waterToLightMap;
        this.lightToTemperatureMap = builder.lightToTemperatureMap;
        this.temperatureToHumidityMap = builder.temperatureToHumidityMap;
        this.humidityToLocationMap = builder.humidityToLocationMap;
    }

    public long getSolutionPt1() {
        Long result = Long.MAX_VALUE;
        System.out.println("Calculating!");

        for (Long seed : seeds) {
            Long soil = seedToSoilMap.getDestination(seed);
            Long fertilizer = soilToFertilizerMap.getDestination(soil);
            Long water = fertilizerToWaterMap.getDestination(fertilizer);
            Long light = waterToLightMap.getDestination(water);
            Long temperature = lightToTemperatureMap.getDestination(light);
            Long humidity = temperatureToHumidityMap.getDestination(temperature);
            Long location = humidityToLocationMap.getDestination(humidity);
//            System.out.printf("%d %d %d %d %d %d %d %d", seed, soil, fertilizer, water, light, temperature, humidity, location);
//            System.out.println();

            if (location < result) {
                result = location;
            }
        }
        return result;
    }

    public long getSolutionPt2() {
        return -1;
    }

    public static Almanac createFromInput(List<String> input) {
        AlmanacBuilder builder = new AlmanacBuilder();
        int parsePhase = 0;
        List<String> buffer = new ArrayList<>();
        for (String inputLine : input) {
            if (inputLine.isEmpty()) {
                processBuffer(parsePhase, buffer, builder);
                buffer.clear();
            } else if (inputLine.endsWith("map:")) {
                parsePhase++;
            } else {
                buffer.add(inputLine);
            }
        }
        processBuffer(parsePhase, buffer, builder);

        return new Almanac(builder);
    }

    private static void processBuffer(int parsePhase, List<String> buffer, AlmanacBuilder builder) {
        switch (parsePhase) {
            case SEED_PHASE:
                builder.setSeeds(buffer.get(0));
                break;
            case SEED_TO_SOIL_PHASE:
                builder.setSeedToSoilMap(buffer);
                break;
            case SOIL_TO_FERTILIZER_PHASE:
                builder.setSoilToFertilizerMap(buffer);
                break;
            case FERTILIZER_TO_WATER_PHASE:
                builder.setFertilizerToWaterMap(buffer);
                break;
            case WATER_TO_LIGHT_PHASE:
                builder.setWaterToLightMap(buffer);
                break;
            case LIGHT_TO_TEMPERATURE_PHASE:
                builder.setLightToTemperatureMap(buffer);
                break;
            case TEMPERATURE_TO_HUMIDITY_PHASE:
                builder.setTemperatureToHumidityMap(buffer);
                break;
            case HUMIDITY_TO_LOCATION_PHASE:
                builder.setHumidityToLocationMap(buffer);
                break;
            default:
                // Should not happen.
                break;
        }
    }
}

