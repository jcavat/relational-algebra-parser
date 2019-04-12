package ch.hepia

import fastparse.{CharIn, P}
import fastparse._
import NoWhitespace._
import Ast._
import ch.hepia.Ast.LogicOp.Cond

object Parser {
  def value[_: P]: P[Value] = CharIn("a-zA-Z0-9").rep(1).!.map( Value )

  def idName[_: P] = CharIn("a-z").rep(1)

  def capitalizedIdName[_: P] = CharIn("A-Z") ~ CharIn("a-z").rep

  def attributeName[_: P] = P( idName.! ).map( AttributeId )

  def relationName[_: P]: P[Relation] = P( capitalizedIdName.! ).map(n => Ast.Relation.SingleRelation(RelationalId(n)))

  def funcArguments[_: P] =
    P( " ".rep ~ attributeName.!.rep(sep=" ".rep ~ "," ~ " ".rep./) ).map(seqs => seqs.map( AttributeId ) )

  def eqJoinCond[_: P] = P(
    for {
      _ <- P(" join(")
      a1 <- attributeName
      _ <- P(" = ")
      a2 <- attributeName
      _ <- P(") ")
    } yield Relation.JoinCond(a1, Op.Eq, a2)
  )

  def joinExpr[_: P]: P[Relation] = P(relationName ~ eqJoinCond ~ relationName)
    .map { case (left, cond, right) => Ast.Relation.Join(left,cond,right) }

  def neqSign[_: P]: P[Op] = P("!=").!.map( _ => Op.Eq )
  def eqSign[_: P]: P[Op] = P("=").!.map( _ => Op.Eq )
  def bigSign[_: P]: P[Op] = P(">").!.map( _ => Op.Big )
  def lessSign[_: P]: P[Op] = P("<").!.map( _ => Op.Less )
  def lessEqSign[_: P]: P[Op] = P("<=").!.map( _ => Op.LessEq )
  def bigEqSign[_: P]: P[Op] = P(">=").!.map( _ => Op.BigEq )
  def sign[_: P]: P[Op] = P( " " ~ (neqSign|eqSign|bigEqSign|lessEqSign|bigSign|lessSign) ~ " " )

  def comparisonExpr[_: P]: P[Cond] = P( attributeName ~ sign ~ value ).map { case (a, s, v) => Cond(a, s, v) }

  def logicFactor[_: P]: P[LogicOp] = comparisonExpr
  def orTerm[_: P]: P[LogicOp] = P( logicFactor ~ " or " ~ logicTerm ).map {case (t, lf) => Ast.LogicOp.Or(t,lf)}
  def logicTerm[_: P]: P[LogicOp] = P( orTerm | logicFactor )
  def andTerm[_: P]: P[LogicOp] = P( logicTerm ~ " and " ~ logicExpr ).map {case (t, lf) => Ast.LogicOp.And(t,lf)}
  def logicExpr[_: P]: P[LogicOp] = P( andTerm | logicTerm )

  def sigmaExpr[_: P]: P[Relation] = P( "sigma(" ~ logicExpr ~ ")(" ~ relationExpr ~ ")" ).map { case (logicOp, rel) => Relation.Sigma(logicOp, rel) }

  def relationExpr[_: P] = P( sigmaExpr|joinExpr|relationName )

  def piExpr[_: P] = P("pi(" ~ funcArguments ~ ")(" ~ relationExpr ~ ")").map { case (attrs, rel) => PiExpr(attrs, rel) }

  def parseAlgebra[_: P]: P[Ast] = P(piExpr|relationExpr)
}
