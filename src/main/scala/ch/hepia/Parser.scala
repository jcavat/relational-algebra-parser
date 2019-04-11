package ch.hepia

import fastparse.{CharIn, P}
import fastparse._, NoWhitespace._
import Ast._

object Parser {
  def value[_: P]: P[Value] = CharIn("a-zA-Z0-9").rep(1).!.map( Value )

  def idName[_: P] = CharIn("a-z").rep(1)

  def capitalizedIdName[_: P] = CharIn("A-Z") ~ CharIn("a-z").rep

  def attributeName[_: P] = P( Parser.idName.! ).map( AttributeId )

  def relationName[_: P]: P[Relation] = P( Parser.capitalizedIdName.! ).map(n => Ast.Relation.SingleRelation(RelationalId(n)))

  def funcArguments[_: P] =
    P( " ".rep ~ Parser.attributeName.!.rep(sep=" ".rep ~ "," ~ " ".rep./) ).map(seqs => seqs.map( AttributeId ) )

  def eqJoinCond[_: P] = P(
    for {
      _ <- P(" join(")
      a1 <- Parser.attributeName
      _ <- P(" = ")
      a2 <- Parser.attributeName
      _ <- P(") ")
    } yield Relation.JoinCond(a1, Op.Eq, a2)
  )

  def joinExpr[_: P]: P[Relation] = P(Parser.relationName ~ Parser.eqJoinCond ~ Parser.relationName)
    .map { case (left, cond, right) => Ast.Relation.Join(left,cond,right) }

  def eqExpr[_: P]: P[Cond] = P( Parser.attributeName ~ " = " ~ Parser.value ).map { case (a, v) => Cond(a, Op.Eq, v) }

  def sigmaExpr[_: P]: P[Relation] = P( "sigma(" ~ Parser.eqExpr ~ ")" ~ relExpr ).map { case (eqE, rel) => Relation.Sigma(eqE, rel) }

  def relExpr[_: P] = P("(" ~ (Parser.sigmaExpr|Parser.joinExpr|Parser.relationName) ~ ")")

  def piArguments[_: P] = P("pi(" ~ Parser.funcArguments ~ ")" ~ Parser.relExpr).map { case (attrs, rel) => PiExpr(attrs, rel) }
}
