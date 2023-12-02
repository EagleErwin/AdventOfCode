package nl.e86.aoc.aoc2023.day02;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Game {
    private static final int MAX_RED = 12;
    private static final int MAX_GREEN = 13;
    private static final int MAX_BLUE = 14;

    private final int id;
    private final List<Draw> draws = new ArrayList<>();

    private Game(int id) {
        // Prevent instantiation. Use createFromString(String);
        this.id = id;
    }

    public int getId() {
        return this.id;
    }

    public boolean isValid() {
        boolean result = true;
        for (Draw currentDraw : draws) {
            if (currentDraw.red > MAX_RED || currentDraw.green > MAX_GREEN || currentDraw.blue > MAX_BLUE) {
                result = false;
                break;
            }
        }
        return result;
    }

    /**
     * Returns the fewest number of cubes that makes this game possible.
     * @return the fewest number of cubes.
     */
    public int calculatePower() {
        int minRed = 0;
        int minGreen = 0;
        int minBlue = 0;
        for (Draw currentDraw : draws) {
            if (currentDraw.red > minRed) {
                minRed = currentDraw.red;
            }
            if (currentDraw.green > minGreen) {
                minGreen = currentDraw.green;
            }
            if (currentDraw.blue > minBlue) {
                minBlue = currentDraw.blue;
            }
        }

        return minRed * minGreen * minBlue;
    }

    public static Game createFromString(String inputLine) {
        Game result;
        // Game 60: 6 blue; 11 blue, 2 red, 6 green; 1 red, 3 blue; 2 green, 1 blue, 2 red
        Pattern idPattern = Pattern.compile("^Game (\\d+): (.*)$");
        Matcher idMatcher = idPattern.matcher(inputLine);
        if (idMatcher.find()) {
            int gameId = Integer.parseInt(idMatcher.group(1));
            String draws = idMatcher.group(2);
            result = new Game(gameId);

            for (String drawString : draws.split(";")) {
                result.addDraw(drawString);
            }
        } else {
            throw new IllegalArgumentException("Unable to find match of " + idPattern + " in " + inputLine);
        }
        return result;
    }

    private void addDraw(String draw) {
        Draw currentDraw = Draw.fromDrawString(draw);
        draws.add(currentDraw);
    }

    private static final class Draw {
        private static final Pattern PATTERN = Pattern.compile("^(\\d+) (red|green|blue)$");
        private final int red;
        private final int green;
        private final int blue;

        private Draw(int red, int green, int blue) {
            // Prevent instantiation
            this.red = red;
            this.green = green;
            this.blue = blue;
        }

        public static Draw fromDrawString(String input) {
            // 11 blue, 2 red, 6 green
            int red = 0;
            int green = 0;
            int blue = 0;
            for (String cubes : input.split(",")) {
                Matcher matcher = PATTERN.matcher(cubes.trim());
                if (matcher.find()) {
                    int amount = Integer.parseInt(matcher.group(1));
                    String color = matcher.group(2);

                    switch (color) {
                        case "red":
                            red = amount;
                            break;
                        case "green":
                            green = amount;
                            break;
                        case "blue":
                            blue = amount;
                            break;
                        default:
                            throw new IllegalArgumentException("Unknown color: " + color);
                    }
                } else {
                    throw new IllegalArgumentException("Unable to find match of " + PATTERN + " in " + cubes);
                }
            }
            return new Draw(red, green, blue);
        }
    }
}
