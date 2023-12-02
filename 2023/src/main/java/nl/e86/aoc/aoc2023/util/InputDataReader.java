package nl.e86.aoc.aoc2023.util;

import nl.e86.aoc.aoc2023.day01.Day01;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class InputDataReader {
    public static List<String> readInput(String filename) {
        List<String> input = Collections.emptyList();
        URL resource = Day01.class.getClassLoader().getResource(filename);

        if (resource == null) {
            System.err.println("Unable to open resource " + filename);
        } else {
            try (InputStream iStream = resource.openStream();
                 BufferedReader reader = new BufferedReader(new InputStreamReader(iStream))
            ) {
                input = getInputString(reader);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

        }

        return input;
    }

    private static List<String> getInputString(BufferedReader reader) {
        List<String> output = new ArrayList<>();
        try {
            while (reader.ready()) {
                output.add(reader.readLine());
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return output;
    }
}
