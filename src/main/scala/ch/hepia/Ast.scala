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

  sealed trait Relation extends Ast
  object Relation {
    case class SingleRelation(name: RelationalId) extends Relation
    case class Join(left: Relation, cond: JoinCond, right: Relation) extends Relation
    case class JoinCond(left: AttributeId, op: Op, right: AttributeId) extends Relation
    case class Sigma(cond: LogicOp, relation: Relation) extends Relation


    def toSql(relation: Relation, placeBefore: String = ""): String = relation match {
      case SingleRelation(id) => id.name
      case Join(left, cond, Join(l,c,r)) =>
        toSql(left) + " INNER JOIN " + toSql(l) + " ON " + toSql(cond) + " INNER JOIN " + toSql(r, " ON " + toSql(c))
      case Join(left, cond, right) => toSql(left) + placeBefore + " INNER JOIN " + toSql(right) + " ON " + toSql(cond)
      case JoinCond(left, cond, right) => left.name + ShowSql[Op].showSql(cond) + right.name
      case Sigma(cond, rel) =>  toSql(rel) + " WHERE " + ShowSql[LogicOp].showSql(cond)
    }

    implicit val canShow = ShowSql[Relation]( r => toSql(r, "") )

  }

  case class PiExpr(attributes: Seq[AttributeId], relation: Relation) extends Ast

  implicit val canShow = ShowSql[Ast]{
    case PiExpr(attrs, rel) => "SELECT " + attrs.map(a => a.name).mkString(", ") + " FROM " + ShowSql[Relation].showSql(rel)
    case o => o.toString
  }

}
