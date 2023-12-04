package nl.e86.aoc.aoc2023.day04;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ScratchCard {
    private static final Pattern CARD_NUMBER_PATTERN = Pattern.compile("Card\\s*(\\d+):");
    private static final Pattern WINNING_NUMBERS_PATTERN = Pattern.compile(":(.*)\\|");
    private static final Pattern NUMBERS_ON_CARD_PATTERN = Pattern.compile("\\|(.*)$");
    private final Integer cardNumber;
    private final List<Integer> winningNumbers;
    private final List<Integer> numbersOnCard;

    private ScratchCard(Integer cardNumber, List<Integer> winningNumbers, List<Integer> numbersOnCard) {
        this.cardNumber = cardNumber;
        this.winningNumbers = Collections.unmodifiableList(winningNumbers);
        this.numbersOnCard = Collections.unmodifiableList(numbersOnCard);
    }

    public int getScore() {
        int score = 0;
        for (Integer number : numbersOnCard) {
            if (winningNumbers.contains(number)) {
                if (score == 0) {
                    score = 1;
                } else {
                    score *= 2;
                }
            }
        }
        return score;
    }

    public static ScratchCard createCardFromInputData(String input) {
        Matcher cardNumberMatcher = CARD_NUMBER_PATTERN.matcher(input);
        cardNumberMatcher.find();
        Integer cardNumber = Integer.parseInt(cardNumberMatcher.group(1));

        Matcher winningNumbersMatcher = WINNING_NUMBERS_PATTERN.matcher(input);
        winningNumbersMatcher.find();
        List<Integer> winningNumbers = new ArrayList<>();
        for (String number : winningNumbersMatcher.group(1).split(" ")) {
            if (!number.isEmpty()) {
                winningNumbers.add(Integer.parseInt(number));
            }
        }

        Matcher numbersOnCardMatcher = NUMBERS_ON_CARD_PATTERN.matcher(input);
        numbersOnCardMatcher.find();
        List<Integer> numbersOnCard = new ArrayList<>();
        for (String number : numbersOnCardMatcher.group(1).split(" ")) {
            if (!number.isEmpty()) {
                numbersOnCard.add(Integer.parseInt(number));
            }
        }

        return new ScratchCard(cardNumber, winningNumbers, numbersOnCard);
    }
}
