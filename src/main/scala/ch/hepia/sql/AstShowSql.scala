package ch.hepia.sql

import ch.hepia.Ast
import ch.hepia.Ast._
import ch.hepia.Ast.Operator._
import ch.hepia.Ast.BooleanOperator._
import ch.hepia.Ast.Relation._

object AstShowSql {

  implicit val operatorCanShowSql = ShowSql[Operator]{
    case Neq => " <> "
    case Eq => " = "
    case Big => " > "
    case Less => " < "
    case BigEq => " >= "
    case LessEq => " <= "
  }

  def toSql(logicOp: BooleanOperator): String = logicOp match {
    case And(left, right) => toSql(left) + " AND " + toSql(right)
    case Or(left, right) => toSql(left) + " OR " + toSql(right)
    case Cond(left, op, right)  => left.name + ShowSql[Operator].showSql(op) + right.value
  }
  implicit val booleanCanShowSql = ShowSql[BooleanOperator]( toSql )

  object JoinCond {
    def toSql(joinCond: JoinCond): String = {
      joinCond.left.name + ShowSql[Operator].showSql(joinCond.op) + joinCond.right.name
    }
  }

  def toSql(relation: Relation): String = relation match {
    case SingleRelation(id) => id.name
    case RelationExpr(sr, joined @ _* ) => toSql(sr) + joined.map { case (jc, rel) => "\nINNER JOIN " + toSql(rel) + " ON " + JoinCond.toSql(jc) }.mkString
    case Sigma(cond, rel) =>  toSql(rel) + "\nWHERE " + ShowSql[BooleanOperator].showSql(cond)
  }
  implicit val relationCanShowSql = ShowSql[Relation]( r => toSql(r) )

  implicit val AstcanShowSql = ShowSql[Ast]{
    case PiExpr(attrs, rel) => "SELECT " + attrs.map(a => a.name).mkString(", ") + "\nFROM " + ShowSql[Relation].showSql(rel)
    case Sigma(cond, rel) => "SELECT * \nFROM " + ShowSql[Relation].showSql(rel) + "\nWHERE " + ShowSql[BooleanOperator].showSql(cond)
    case RelationExpr(sr, joined @ _* ) => "SELECT * \nFROM " + toSql(sr) + joined.map { case (jc, rel) => "\nINNER JOIN " + toSql(rel) + " ON " + JoinCond.toSql(jc) }.mkString
    case o => o.toString
  }



}
