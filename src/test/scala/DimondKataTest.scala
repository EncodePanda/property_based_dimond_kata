
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._
import DimondKata._

class DimondKataTest extends FunSuite with Matchers with PropertyChecks {

  val char = Gen.alphaUpperChar

  def charIdx(c: Char) = c.toInt - 'A'.toInt

  test("property: dimond's number of lines equal to twice char index + 1") {
    forAll(char) { ch =>
      dimond(ch).size should equal((charIdx(ch) * 2)+1)
    }
  }

}
