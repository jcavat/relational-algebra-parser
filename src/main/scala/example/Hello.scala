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
      case class Join(left: Relation, cond: Cond, right: Relation) extends Relation
      case class Cond(left: AttributeId, op: Op, right: AttributeId) extends Relation
      case class Sigma(cond: Cond, relation: Relation) extends Relation
    }

    case class PiExpr(attributes: Seq[AttributeId], relation: Relation)
  }

  import Ast._

  def idName[_: P] = CharIn("a-z").rep(1)
  def capitalizedIdName[_: P] = CharIn("A-Z") ~ CharIn("a-z").rep

  def attributeName[_: P] = P( idName.! ).map( AttributeId(_) )
  def relationName[_: P]: P[Relation] = P( capitalizedIdName.! ).map( n => Ast.Relation.SingleRelation(RelationalId(n)))

  def funcArguments[_: P] = P( " ".rep ~ attributeName.!.rep(sep=" ".rep ~ "," ~ " ".rep./) ).map( seqs => seqs.map(AttributeId(_)) )
  def eqJoinCond[_: P] = P(
    for {
      _ <- P(" join(")
      a1 <- attributeName
      _ <- P(" = ")
      a2 <- attributeName
      _ <- P(") ")
    } yield Relation.Cond(a1, Op.Eq, a2)
  )

  def joinExpr[_: P]: P[Relation] = P(relationName ~ eqJoinCond ~ relationName)
    .map { case (left, cond, right) => Ast.Relation.Join(left,cond,right) }

  def eqExpr[_: P]: P[Relation.Cond] = P( attributeName ~ " = " ~ attributeName ).map { case (a1, a2) => Relation.Cond(a1, Op.Eq, a2) }
  def sigmaExpr[_: P]: P[Relation] = P( "sigma(" ~ eqExpr ~ ")" ~ relExpr ).map { case (eqE, rel) => Relation.Sigma(eqE, rel) }

  def relExpr[_: P] = P("(" ~ (sigmaExpr|joinExpr|relationName) ~ ")")
  def piArguments[_: P] = P("pi(" ~ funcArguments ~ ")" ~ relExpr).map { case (attrs, rel) => PiExpr(attrs, rel) }

  val Parsed.Success(value, successIndex) = parse("pi(test, coucou)(sigma(t = k)(Person join(u = b) Voiture))", piArguments(_))
  
  println(value)
}
