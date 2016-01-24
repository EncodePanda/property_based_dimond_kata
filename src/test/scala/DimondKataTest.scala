
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

class DimondKataTest extends FunSuite with Matchers with PropertyChecks {

  val dimonds = for {
    char <- Gen.alphaUpperChar
  } yield (char, DimondKata.dimond(char))


  def charIdx(c: Char) = c.toInt - 'A'.toInt

  test("property: dimond's number of lines equal to twice char index + 1") {
    forAll(dimonds) { case (char, dimond) =>
      dimond.size should equal((charIdx(char) * 2) + 1)
    }
  }

  test("property: dimond has a non-zero number of lines") {
    forAll(dimonds) { case (char, dimond) =>
      dimond.size should not(equal(0))
    }
  }

  test("property: all dimond's lines are of the same size") {
    forAll(dimonds) { case (char, dimond) =>
      dimond.foreach { line =>
        line.size should equal(dimond(0).size)
      }
    }
  }

  test("property: dimond's single line size is equal dimond's number of lines (square)") {
    forAll(dimonds) { case (char, dimond) =>
      dimond.foreach { line =>
        line.size should equal(dimond.size)
      }
    }
  }



}
