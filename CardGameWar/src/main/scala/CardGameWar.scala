import scala.util.Random

case class Card(suit: String, rank: String)

case class Deck(cards: List[Card])

case class Player(name: String, deck: Deck)

object CardGameWar {
  // Feel free to use these cards or use your own data structure
  val suits = List("Spade", "Club", "Diamond", "Heart")
  val ranks = List("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")

  // Creates two shuffled decks of cards
  def createDecks: (Deck, Deck) = {
    val allCards =
      new Random shuffle (for {
        suit <- suits
        rank <- ranks
      } yield Card(suit, rank.toString))

    val List(d1, d2) = allCards.grouped(allCards.length / 2).toList
    (Deck(d1), Deck(d2))
  }

  def playRound(player1: Card, player2: Card): Card = {
    val r1 = ranks.indexOf(player1.rank)
    val r2 = ranks.indexOf(player2.rank)
    val s1 = suits.indexOf(player1.suit)
    val s2 = suits.indexOf(player2.suit)
    if (r1 > r2 || (r1 == r2 && s1 > s2))
      player1
    else
      player2
  }

  def playGame(player1: Player, player2: Player): String = {
    ??? // Winner of Game
  }

}

