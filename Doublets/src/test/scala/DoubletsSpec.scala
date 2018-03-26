import Doublets._
import org.scalatest._

class DoubletsSpec extends WordSpec {
  "Doublets" when {
    "with word links found" in {
      assert(solutionIsCorrect(doublets("door", "lock")))
      assert(solutionIsCorrect(doublets("bank", "loan")))
      assert(solutionIsCorrect(doublets("wheat", "bread")))
    }
    "with no word links found" in {
      assert(doublets("ye", "freezer") == Seq())
    }
  }
}
