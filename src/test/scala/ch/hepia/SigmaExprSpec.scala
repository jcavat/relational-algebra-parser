package ch.hepia

import ch.hepia.Ast.BooleanOperator.{And, Cond, Or}
import ch.hepia.Ast.Relation.{RelationExpr, Sigma, SingleRelation}
import ch.hepia.Ast._
import ch.hepia.Parser.parseAlgebra
import fastparse.{Parsed, _}
import org.scalatest._

class SigmaExprSpec extends FlatSpec with Matchers {
  "Sigma expr with conditions" should "succeed with equality" in {
    val Parsed.Success(value, _) = parse("sigma(city = Lausanne)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        Cond(AttributeId("city"), Operator.Eq, Value("Lausanne")),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
  "Sigma expr with conditions" should "succeed with greater or equals" in {
    val Parsed.Success(value, _) = parse("sigma(age <= 18)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        Cond(AttributeId("age"), Operator.LessEq, Value("18")),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
  "Sigma expr with conditions" should "succeed with less or equals" in {
    val Parsed.Success(value, _) = parse("sigma(age >= 18)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        Cond(AttributeId("age"), Operator.BigEq, Value("18")),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
  "Sigma expr with conditions" should "succeed with less" in {
    val Parsed.Success(value, _) = parse("sigma(age < 18)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        Cond(AttributeId("age"), Operator.Less, Value("18")),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
  "Sigma expr with conditions" should "succeed with greater" in {
    val Parsed.Success(value, _) = parse("sigma(age > 18)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        Cond(AttributeId("age"), Operator.Big, Value("18")),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
  "Sigma expr with conditions" should "succeed with and" in {
    val Parsed.Success(value, _) = parse("sigma(age > 18 and name = david)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        And(
          Cond(AttributeId("age"), Operator.Big, Value("18")),
          Cond(AttributeId("name"), Operator.Eq, Value("david"))
        ),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
  "Sigma expr with conditions" should "succeed with or" in {
    val Parsed.Success(value, _) = parse("sigma(age > 18 or name = david)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        Or(
          Cond(AttributeId("age"), Operator.Big, Value("18")),
          Cond(AttributeId("name"), Operator.Eq, Value("david"))
        ),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
  "Sigma expr with conditions" should "succeed with and/or" in {
    val Parsed.Success(value, _) = parse("sigma(age > 18 and name = david or lastname = jackson)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        And(
          Cond(AttributeId("age"), Operator.Big, Value("18")),
          Or(
            Cond(AttributeId("name"), Operator.Eq, Value("david")),
            Cond(AttributeId("lastname"), Operator.Eq, Value("jackson"))
          )
        ),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
  "Sigma expr with conditions" should "succeed with or/and" in {
    val Parsed.Success(value, _) = parse("sigma(age > 18 or name = david and lastname = jackson)(Person)", parseAlgebra(_))
    value should be (
      Sigma(
        And(
          Or(
            Cond(AttributeId("age"), Operator.Big, Value("18")),
            Cond(AttributeId("name"), Operator.Eq, Value("david"))
          ),
          Cond(AttributeId("lastname"), Operator.Eq, Value("jackson"))
        ),
        RelationExpr(
          SingleRelation(RelationalId("Person")))
        )
    )
  }
}
