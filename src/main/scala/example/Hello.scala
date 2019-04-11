package example



object Hello extends App {
  import fastparse._, NoWhitespace._

  object Ast {

    case class RelationalId(name: String)
    case class AttributeId(name: String)

    sealed trait Op
    object Op {
      case object Eq extends Op
    }

    sealed trait Relation
    object Relation {
      case class SingleRelation(name: RelationalId) extends Relation
      case class Join(left: SingleRelation, cond: Cond, right: SingleRelation) extends Relation
      case class Cond(left: String, op: Op, right: String) extends Relation
    }
  }

  import Ast._

  def idName[_: P] = CharIn("a-z").rep(1)
  def capitalizedIdName[_: P] = CharIn("A-Z") ~ CharIn("a-z").rep

  def attributeName[_: P] = P( idName.! ).map( AttributeId(_) )
  def relationName[_: P] = P( capitalizedIdName.! ).map( n => Ast.Relation.SingleRelation(RelationalId(n)))

  def funcArguments[_: P] = P( " ".rep ~ attributeName.rep(sep=" ".rep ~ "," ~ " ".rep./) )

  def join[_: P] = ???
  def joinExpr[_: P] = P(relationName ~ " joinExpr " ~ eqJoinCond ~ relationName).map( n => Ast.Relation.Join(n._1, n._2, n._3))
  def eqJoinCond[_: P] = P("on (" ~ idName.! ~ " = " ~ idName.! ~ ") ").map( n => Ast.Relation.Cond(n._1, Op.Eq, n._2) )


  def relExpr[_: P] = P("(" ~ (joinExpr|relationName) ~ ")")
  def piArguments[_: P] = P("pi(" ~ funcArguments ~ ")" ~ relExpr)

  val Parsed.Success(value, successIndex) = parse("pi(test, coucou)(Person joinExpr on (u = b) Voiture)", piArguments(_))
  
  println(value)
}
