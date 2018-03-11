import CardGameWar._
import org.scalatest._

class CardGameWarSpec extends WordSpec with Matchers {
  "playRound" when {
    "the highest rank wins the cards in the round" in {
      assert(playRound( Card("Spade", "2"), Card("Spade","Ace")) == Card("Spade", "Ace"))
    }
    "queens are higher rank than jacks" in {
      assert(playRound( Card("Spade", "Queen"), Card("Spade","Jack")) == Card("Spade", "Queen"))
    }
    "kings are higher rank than queens" in {
      assert(playRound( Card("Spade", "Queen"), Card("Spade","King")) == Card("Spade", "King"))
    }
    "aces are higher rank than kings" in {
      assert(playRound( Card("Spade", "Ace"), Card("Spade","King")) == Card("Spade", "Ace"))
    }
    "if the ranks are equal, clubs beat spades" in {
      assert(playRound( Card("Club", "Ace"), Card("Spade","Ace")) == Card("Club", "Ace"))
    }
    "if the ranks are equal, diamonds beat clubs" in {
      assert(playRound( Card("Club", "Ace"), Card("Diamond","Ace")) == Card("Diamond", "Ace"))
    }
    "if the ranks are equal, hearts beat diamonds" in {
      assert(playRound( Card("Heart", "Ace"), Card("Diamond","Ace")) == Card("Heart", "Ace"))
    }
  }

  "nextRound" when {
    "if the first player wins the top cards are added to the back of their deck" in {
      val player1 = Player("Bob", Deck(List(Card("Spade", "Ace"), Card("Hearts", "King"))))
      val player2 = Player("Alice", Deck(List(Card("Spade", "2"), Card("Hearts", "Queen"))))
      val (newPlayer1, newPlayer2) = nextRound(player1, player2)
      assert(newPlayer1.deck.cards == List(Card("Hearts", "King"), Card("Spade", "Ace"), Card("Spade", "2")))
      assert(newPlayer2.deck.cards == List(Card("Hearts", "Queen")))
    }

    "if the second player wins the top cards are added to the back of their deck" in {
      val player1 = Player("Bob", Deck(List(Card("Spade", "2"), Card("Hearts", "King"))))
      val player2 = Player("Alice", Deck(List(Card("Spade", "Ace"), Card("Hearts", "Queen"))))
      val (newPlayer1, newPlayer2) = nextRound(player1, player2)
      assert(newPlayer1.deck.cards == List(Card("Hearts", "King")))
      assert(newPlayer2.deck.cards == List(Card("Hearts", "Queen"), Card("Spade", "Ace"), Card("Spade", "2")))
    }
  }

  "playGame" when {
    "the player loses when they run out of cards" in {
      val (deck1, deck2) = createDecks
      val player1 = Player("Bob", deck1)
      val player2 = Player("Alice", deck2)
      assert(playGame(player1, player2).isInstanceOf[String])
    }
  }
}
