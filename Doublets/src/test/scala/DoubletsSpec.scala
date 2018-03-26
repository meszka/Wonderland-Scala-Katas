import Doublets._
import org.scalatest._

class DoubletsSpec extends WordSpec {
  "Doublets" when {
    "with word links found" in {
      //assert(doublets("head", "tail") == Seq("head","heal","teal","tell","tall","tail"))
      //assert(doublets("head", "tail") == Seq("head","heal","hell","tell","tall","tail"))
      assert(doublets("door", "lock") == Seq("door", "boor", "book", "look", "lock"))
      //assert(doublets("bank", "loan") == Seq("bank", "bonk", "book", "look", "loon", "loan"))
      assert(doublets("bank", "loan") == Seq("bank", "bark", "bork", "born", "lorn", "loan"))
      //assert(doublets("wheat", "bread") == Seq("wheat", "cheat", "cheap", "cheep", "creep", "creed", "breed", "bread"))
      assert(doublets("wheat", "bread") == Seq("wheat", "cheat", "cleat", "bleat", "bleak", "break", "bread"))
    }
    "with no word links found" in {
      assert(doublets("ye", "freezer") == Seq())
    }
  }
}
