package ch.hepia

import Ast.Relation.{Join, JoinCond, Sigma, SingleRelation}
import Ast._
import ch.hepia.Ast.LogicOp.Cond
import ch.hepia.Parser.parseAlgebra
import fastparse.Parsed
import org.scalatest._
import fastparse._

class PiExprSpec extends FlatSpec with Matchers {
  "Pi expr with one relation" should "succeed" in {
    val Parsed.Success(value, _) = parse("pi(test, coucou)(Person)", parseAlgebra(_))
    value should be (
      PiExpr(
        Seq(AttributeId("test"), AttributeId("coucou")),
        SingleRelation(RelationalId("Person")))
    )
  }
  "Pi and sigma expr with one relation" should "succeed" in {
    val Parsed.Success(value, _) = parse("pi(test, coucou)(sigma(a = u)(Person))", parseAlgebra(_))
    value should be (
      PiExpr(
        Seq(AttributeId("test"), AttributeId("coucou")),
        Sigma(
          Cond(AttributeId("a"), Ast.Op.Eq, Value("u")),
          SingleRelation(RelationalId("Person"))
        )
      )
    )
    val Parsed.Success(value2, _) = parse("pi(name)(sigma(age = 18)(Person))", parseAlgebra(_))
    value2 should be (
      PiExpr(
        Seq(AttributeId("name")),
        Sigma(
          Cond(AttributeId("age"), Ast.Op.Eq, Value("18")),
          SingleRelation(RelationalId("Person"))
        )
      )
    )
  }
  "Pi expr with one join" should "succeed" in {
    val Parsed.Success(value, _) = parse("pi(name, immat)(Person join(id = uid) Car)", parseAlgebra(_))
    value should be (
      PiExpr(
        Seq(AttributeId("name"), AttributeId("immat")),
        Join(
          SingleRelation(RelationalId("Person")),
          JoinCond(AttributeId("id"), Op.Eq, AttributeId("uid")),
          SingleRelation(RelationalId("Car"))
        )
      )
    )
  }
  "Pi expr and sigma with one join" should "succeed" in {
    val Parsed.Success(value, _) = parse("pi(name, immat)(sigma(color = red)(Person join(id = uid) Car))", parseAlgebra(_))
    value should be (
      PiExpr(
        Seq(AttributeId("name"), AttributeId("immat")),
        Sigma(
          Cond(AttributeId("color"), Ast.Op.Eq, Value("red")),
          Join(
            SingleRelation(RelationalId("Person")),
            JoinCond(AttributeId("id"), Op.Eq, AttributeId("uid")),
            SingleRelation(RelationalId("Car"))
          )
        )
      )
    )
  }
}
