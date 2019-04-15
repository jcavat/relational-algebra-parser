package ch.hepia

sealed trait Ast
object Ast {

  case class RelationalId(name: String) extends Ast
  case class AttributeId(name: String) extends Ast
  case class Value(value: String) extends Ast

  sealed trait Op extends Ast
  object Op {
    case object Neq extends Op
    case object Eq extends Op
    case object Big extends Op
    case object Less extends Op
    case object BigEq extends Op
    case object LessEq extends Op
    implicit val canShow = ShowSql[Op]{
      case Neq => " <> "
      case Eq => " = "
      case Big => " > "
      case Less => " < "
      case BigEq => " >= "
      case LessEq => " <= "
    }
  }

  sealed trait LogicOp extends Ast
  object LogicOp {
    case class And(left: LogicOp, right: LogicOp) extends LogicOp
    case class Or(left: LogicOp, right: LogicOp) extends LogicOp
    case class Cond(left: AttributeId, op: Op, right: Value) extends LogicOp

    def toSql(logicOp: LogicOp): String = logicOp match {
      case And(left, right) => toSql(left) + " AND " + toSql(right)
      case Or(left, right) => toSql(left) + " OR " + toSql(right)
      case Cond(left, op, right)  => left.name + ShowSql[Op].showSql(op) + right.value
    }

    implicit val canShow = ShowSql[LogicOp]( toSql )
  }

  case class JoinCond(left: AttributeId, op: Op, right: AttributeId) {

  }

  object JoinCond {
    def toSql(joinCond: JoinCond): String = {
      joinCond.left.name + ShowSql[Op].showSql(joinCond.op) + joinCond.right.name
    }
  }

  // ... Sigma( A join B join C )
  // .... A join B join C

  /*
   * Relation ::= SigmaExp | RelationExpr
   * SigmaExpr ::= Sigma( RelationExpr )
   * RelationExpr ::= SingleRelation { Joined }
   * Joined ::= Joined(Relation, Condition)
   */

  sealed trait Relation extends Ast
  object Relation {
    case class SingleRelation(name: RelationalId) extends Relation
    case class Sigma(cond: LogicOp, relation: Relation) extends Relation
    case class RelationExpr(singleRelation: SingleRelation, joined: (JoinCond, Relation)*) extends Relation


    def toSql(relation: Relation): String = relation match {
      case SingleRelation(id) => id.name
      case RelationExpr(sr, joined @ _* ) => toSql(sr) + joined.map { case (jc, rel) => "\nINNER JOIN " + toSql(rel) + " ON " + JoinCond.toSql(jc) }.mkString
      case Sigma(cond, rel) =>  toSql(rel) + "\nWHERE " + ShowSql[LogicOp].showSql(cond)
    }

    implicit val canShow = ShowSql[Relation]( r => toSql(r) )

  }

  case class PiExpr(attributes: Seq[AttributeId], relation: Relation) extends Ast

  implicit val canShow = ShowSql[Ast]{
    case PiExpr(attrs, rel) => "SELECT " + attrs.map(a => a.name).mkString(", ") + " FROM " + ShowSql[Relation].showSql(rel)
    case o => o.toString
  }

}
