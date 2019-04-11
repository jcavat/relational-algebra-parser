package ch.hepia

object Ast {

  case class RelationalId(name: String)
  case class AttributeId(name: String)
  case class Value(value: String)

  sealed trait Op
  object Op {
    case object Eq extends Op
  }

  sealed trait Relation
  object Relation {
    case class SingleRelation(name: RelationalId) extends Relation
    case class Join(left: Relation, cond: JoinCond, right: Relation) extends Relation
    case class JoinCond(left: AttributeId, op: Op, right: AttributeId) extends Relation
    case class Sigma(cond: Cond, relation: Relation) extends Relation
  }

  case class PiExpr(attributes: Seq[AttributeId], relation: Relation)

  case class Cond(left: AttributeId, op: Op, right: Value) extends Relation

}
