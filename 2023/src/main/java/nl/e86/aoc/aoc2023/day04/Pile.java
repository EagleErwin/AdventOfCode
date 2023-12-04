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
        return winCards();
    }

    private int getScore() {
        int score = 0;
        for (ScratchCard card : cards) {
            score += card.getScore();
        }
        return score;
    }

    private int winCards() {
        int totalCards = 0;
        int cardIndex = 0;
        while (cardIndex < cards.size()) {
            ScratchCard currentCard = cards.get(cardIndex);
            totalCards++;
            int extraCards = currentCard.getExtraCards();
            for (int i = 1; i <= extraCards; ++i) {
                ScratchCard extraCard = duplicateCard(currentCard.getCardNumber() + i);
                cards.add(extraCard);
            }
            cardIndex++;
        }
        return totalCards;
    }

    private ScratchCard duplicateCard(int cardId) {
        ScratchCard match = null;
        for (ScratchCard card : cards) {
            if (card.getCardNumber() == cardId) {
                match = card.getCopy();
                break;
            }
        }

        return match;
    }

    public static Pile createFromInput(List<String> input) {
        List<ScratchCard> cards = new ArrayList<>();
        for (String inputLine : input) {
            cards.add(ScratchCard.createCardFromInputData(inputLine));
        }
        return new Pile(cards);
    }
}
