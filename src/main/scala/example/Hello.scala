package example



object Hello extends App {
  import fastparse._, NoWhitespace._

  case class Identifier(name: String)

  object Ast {
    sealed trait Relation
    object Relation {
      case class SingleRelation(name: Identifier) extends Relation
      case class Join(left: SingleRelation, right: SingleRelation) extends Relation
    }
  }
  def argName[_: P] = CharIn("a-z").rep(1)
  def relName[_: P] = CharIn("A-Z") ~ CharIn("a-z").rep
  def arguments[_: P] = P( " ".rep ~ argName.!.rep(sep=" ".rep ~ "," ~ " ".rep./) )
  def relation[_: P] = P(relName.!.map( n => Ast.Relation.SingleRelation(Identifier(n))))
  def join[_: P] = P(relation ~ " join " ~ relation).map( n => Ast.Relation.Join(n._1, n._2))
  def relExpr[_: P] = P("(" ~ (join|relation) ~ ")")
  def piArguments[_: P] = P("pi(" ~ arguments ~ ")" ~ relExpr)

  val Parsed.Success(value, successIndex) = parse("pi(test, coucou)(Person join Voiture)", piArguments(_))
  
  println(value)
}