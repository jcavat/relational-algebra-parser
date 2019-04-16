package ch.hepia

import ch.hepia.Parser.parseAlgebra
import fastparse.{Parsed, _}
import org.scalatest._

class SqlSpec extends FlatSpec with Matchers {
  import ch.hepia.sql.ShowSql._
  import ch.hepia.sql.AstShowSql._
  "One relation" should "export to SQL successfully" in {
    val Parsed.Success(value, _) = parse("Person", parseAlgebra(_))
    value.showSql should equal(
      """
        |SELECT *
        |FROM Person
      """.stripMargin.strip)
  }
  "One join" should "export to SQL successfully" in {
    val Parsed.Success(value, _) = parse("Person join(p = c) Car", parseAlgebra(_))
    value.showSql should equal(
      """
        |SELECT *
        |FROM Person
        |INNER JOIN Car ON p = c
      """.stripMargin.strip)
  }
  "Pi expr with one relation" should "export to SQL successfully" in {
    val Parsed.Success(value, _) = parse("pi(firstname, lastname)(Person)", parseAlgebra(_))
    value.showSql should equal(
      """
        |SELECT firstname, lastname
        |FROM Person
      """.stripMargin.strip)
  }
  "Pi expr with one join" should "export to SQL successfully" in {
    val Parsed.Success(value, _) = parse("pi(firstname, lastname)(Person join(p = c) Car)", parseAlgebra(_))
    value.showSql should equal(
      """
        |SELECT firstname, lastname
        |FROM Person
        |INNER JOIN Car ON p = c
      """.stripMargin.strip)
  }
  "Sigma expr with one join" should "export to SQL successfully" in {
    val Parsed.Success(value, _) = parse("sigma(age = 30)(Person join(p = c) Car)", parseAlgebra(_))
    value.showSql should equal(
      """
        |SELECT *
        |FROM Person
        |INNER JOIN Car ON p = c
        |WHERE age = 30
      """.stripMargin.strip)
  }
  "Pi/Sigma expr with one join" should "export to SQL successfully" in {
    val Parsed.Success(value, _) = parse("pi(firstname, age)(sigma(age > 18)(Person join(p = c) Car))", parseAlgebra(_))
    value.showSql should equal(
      """
        |SELECT firstname, age
        |FROM Person
        |INNER JOIN Car ON p = c
        |WHERE age > 18
      """.stripMargin.strip)
  }
}
