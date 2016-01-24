
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._
import DimondKata.Line

class DimondKataTest extends FunSuite with Matchers with PropertyChecks {

  val chars = Gen.alphaUpperChar

  def charIdx(c: Char) = c.toInt - 'A'.toInt

  test("property: dimond's number of lines equal to twice char index + 1") {
    property { case (char, dimond) =>
      dimond.size should equal((charIdx(char) * 2) + 1)
    }
  }

  test("property: dimond has a non-zero number of lines") {
    property { case (char, dimond) =>
      dimond.size should not(equal(0))
    }
  }

  test("property: all dimond's lines are of the same size") {
    property { case (char, dimond) =>
      dimond.foreach { line =>
        line.size should equal(dimond(0).size)
      }
    }
  }

  test("property: dimond's single line size is equal dimond's number of lines (square)") {
    property { case (char, dimond) =>
      dimond.foreach { line =>
        line.size should equal(dimond.size)
      }
    }
  }

  

  def property(check: (Char, List[Line]) => Unit) = {
    forAll(chars) { char =>
      check(char, DimondKata.dimond(char))
    }
  }


}
