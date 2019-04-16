package ch.hepia

import ch.hepia.Ast.Relation.{RelationExpr, SingleRelation}
import ch.hepia.Ast._
import ch.hepia.Parser.parseAlgebra
import fastparse.{Parsed, _}
import org.scalatest._

class RelationExprSpec extends FlatSpec with Matchers {
  "Single relation" should "succeed" in {
    val Parsed.Success(value, _) = parse("Person", parseAlgebra(_))
    println(value)
    value should be (
        RelationExpr(
          SingleRelation(RelationalId("Person"))
        )
      )
  }
  "Two joined relations" should "succeed" in {
    val Parsed.Success(value, _) = parse("Person join(p = c) Car", parseAlgebra(_))
    println(value)
    value should be (
      RelationExpr(
        SingleRelation(RelationalId("Person")),
        ( JoinCond(AttributeId("p"), Operator.Eq, AttributeId("c")), SingleRelation(RelationalId("Car")) )
      )
    )
  }
  "Three joined relations" should "succeed" in {
    val Parsed.Success(value, _) = parse("Office join(o = p) Person join(p = c) Car", parseAlgebra(_))
    println(value)
    value should be (
      RelationExpr(
        SingleRelation(RelationalId("Office")),
        ( JoinCond(AttributeId("o"), Operator.Eq, AttributeId("p")), SingleRelation(RelationalId("Person")) ),
        ( JoinCond(AttributeId("p"), Operator.Eq, AttributeId("c")), SingleRelation(RelationalId("Car")) )
      )
    )
  }
}
