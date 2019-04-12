package ch.hepia

object Ast {

  case class RelationalId(name: String)
  case class AttributeId(name: String)
  case class Value(value: String)

  sealed trait Op
  object Op {
    case object Neq extends Op
    case object Eq extends Op
    case object Big extends Op
    case object Less extends Op
    case object BigEq extends Op
    case object LessEq extends Op
  }

  sealed trait LogicOp
  object LogicOp {
    case class And(left: LogicOp, right: LogicOp) extends LogicOp
    case class Or(left: LogicOp, right: LogicOp) extends LogicOp
    case class Cond(left: AttributeId, op: Op, right: Value) extends LogicOp
  }

  sealed trait Relation
  object Relation {
    case class SingleRelation(name: RelationalId) extends Relation
    case class Join(left: Relation, cond: JoinCond, right: Relation) extends Relation
    case class JoinCond(left: AttributeId, op: Op, right: AttributeId) extends Relation
    case class Sigma(cond: LogicOp, relation: Relation) extends Relation
  }

  case class PiExpr(attributes: Seq[AttributeId], relation: Relation)


}
