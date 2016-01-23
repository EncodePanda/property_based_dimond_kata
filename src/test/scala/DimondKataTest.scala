
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._
import DimondKata._

class DimondKataTest extends FunSuite with Matchers with PropertyChecks {

  val char = Gen.alphaUpperChar

  def charIdx(c: Char) = c.toInt - 'A'.toInt

  test("property: dimond's number of lines equal to twice char index + 1") {
    forAll(char) { ch =>
      dimond(ch).size should equal((charIdx(ch) * 2) + 1)
    }
  }

  test("property: dimond has a non-zero number of lines") {
    forAll(char) { ch =>
      dimond(ch).size should not(equal(0))
    }
  }

  test("property: all dimond's lines are of the same size") {
    forAll(char) { ch =>
      val d = dimond(ch)
      d.foreach { line =>
        line.size should equal(d(0).size)
      }
    }
  }

  test("property: dimond's single line size is equal dimond's number of lines (square)") {
    forAll(char) { ch =>
      val d = dimond(ch)
      d.foreach { line =>
        d(0).size should equal(d.size)
      }
    }
  }

}
