import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import Main._

class Test extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "convertToInt" should "convert a String to Int" in {

    val intString = "convertToInt"("1") {
      Or.first("1")
    }
    "intString" should contain(1)
  }

  "bimap" should "transform a String or a Int to another one" in {

    /*val intBimap = "bimap"("1")(_ + 1, _.length) {
      convertToInt("1").bimap(_ + 1, _.length)
    }
    "intBimap" should contain(2)*/
  }

}