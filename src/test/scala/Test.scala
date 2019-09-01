import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class Test extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "convertToInt" should "convert a String to Int" in {

    val intString = convertToInt("1") {
      Some(1)
    }

    intString should contain(1)
  }


}