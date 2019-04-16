package ch.hepia

sealed trait Ast
object Ast {

  case class RelationalId(name: String) extends Ast
  case class AttributeId(name: String) extends Ast
  case class Value(value: String) extends Ast

  sealed trait Operator extends Ast
  object Operator {
    case object Neq extends Operator
    case object Eq extends Operator
    case object Big extends Operator
    case object Less extends Operator
    case object BigEq extends Operator
    case object LessEq extends Operator
  }

  sealed trait BooleanOperator extends Ast
  object BooleanOperator {
    case class And(left: BooleanOperator, right: BooleanOperator) extends BooleanOperator
    case class Or(left: BooleanOperator, right: BooleanOperator) extends BooleanOperator
    case class Cond(left: AttributeId, op: Operator, right: Value) extends BooleanOperator
  }

  case class JoinCond(left: AttributeId, op: Operator, right: AttributeId)

  sealed trait Relation extends Ast
  object Relation {
    case class SingleRelation(name: RelationalId) extends Relation
    case class Sigma(cond: BooleanOperator, relation: Relation) extends Relation
    case class RelationExpr(singleRelation: SingleRelation, joined: (JoinCond, Relation)*) extends Relation
  }

  case class PiExpr(attributes: Seq[AttributeId], relation: Relation) extends Ast

}
