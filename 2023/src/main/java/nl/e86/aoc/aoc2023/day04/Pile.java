package nl.e86.aoc.aoc2023.day04;

import java.util.ArrayList;
import java.util.List;

public class Pile {
    private final List<ScratchCard> cards;
    private Pile(List<ScratchCard> cards) {
        this.cards = cards;
    }

    public int getSolutionPt1() {
        return getScore();
    }
    public int getSolutionPt2() {
        return -1;
    }

    private int getScore() {
        int score = 0;
        for (ScratchCard card : cards) {
            score += card.getScore();
        }
        return score;
    }

    public static Pile createFromInput(List<String> input) {
        List<ScratchCard> cards = new ArrayList<>();
        for (String inputLine : input) {
            cards.add(ScratchCard.createCardFromInputData(inputLine));
        }
        return new Pile(cards);
    }
}
