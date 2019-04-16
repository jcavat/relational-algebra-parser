package ch.hepia

import fastparse.{CharIn, P}
import fastparse._
import NoWhitespace._
import Ast._
import ch.hepia.Ast.BooleanOperator.Cond
import ch.hepia.Ast.Relation.SingleRelation

object Parser {

  def value[_: P]: P[Value] = CharIn("a-zA-Z0-9").rep(1).!.map( Value )

  def idName[_: P] = CharIn("a-z").rep(1)

  def capitalizedIdName[_: P] = CharIn("A-Z") ~ CharIn("a-z").rep

  def attributeName[_: P] = P( idName.! ).map( AttributeId )

  def singleRelation[_: P]: P[SingleRelation] = P( capitalizedIdName.! ).map(n => Ast.Relation.SingleRelation(RelationalId(n)))

  def funcArguments[_: P] =
    P( " ".rep ~ attributeName.!.rep(sep=" ".rep ~ "," ~ " ".rep./) ).map(seqs => seqs.map( AttributeId ) )

  def eqJoinCond[_: P] = P(
    for {
      _ <- P(" join(")
      a1 <- attributeName
      _ <- P(" = ")
      a2 <- attributeName
      _ <- P(") ")
    } yield JoinCond(a1, Operator.Eq, a2)
  )

  def originExpr[_: P]: P[Relation.RelationExpr] =
    P(singleRelation ~ (eqJoinCond ~ singleRelation).rep).map {
      case (sr, joined) => Relation.RelationExpr(sr, joined:_*)
    }

  def neqSign[_: P]: P[Operator] = P("!=").!.map(_ => Operator.Eq )
  def eqSign[_: P]: P[Operator] = P("=").!.map(_ => Operator.Eq )
  def bigSign[_: P]: P[Operator] = P(">").!.map(_ => Operator.Big )
  def lessSign[_: P]: P[Operator] = P("<").!.map(_ => Operator.Less )
  def lessEqSign[_: P]: P[Operator] = P("<=").!.map(_ => Operator.LessEq )
  def bigEqSign[_: P]: P[Operator] = P(">=").!.map(_ => Operator.BigEq )
  def sign[_: P]: P[Operator] = P( " " ~ (neqSign|eqSign|bigEqSign|lessEqSign|bigSign|lessSign) ~ " " )

  def comparisonExpr[_: P]: P[Cond] = P( attributeName ~ sign ~ value ).map { case (a, s, v) => Cond(a, s, v) }

  def logicFactor[_: P]: P[BooleanOperator] = comparisonExpr
  def orTerm[_: P]: P[BooleanOperator] = P( logicFactor ~ " or " ~ logicTerm ).map {case (t, lf) => Ast.BooleanOperator.Or(t,lf)}
  def logicTerm[_: P]: P[BooleanOperator] = P( orTerm | logicFactor )
  def andTerm[_: P]: P[BooleanOperator] = P( logicTerm ~ " and " ~ logicExpr ).map {case (t, lf) => Ast.BooleanOperator.And(t,lf)}
  def logicExpr[_: P]: P[BooleanOperator] = P( andTerm | logicTerm )

  def sigmaExpr[_: P]: P[Relation] = P( "sigma(" ~ logicExpr ~ ")(" ~ originExpr ~ ")" ).map { case (logicOp, rel) => Relation.Sigma(logicOp, rel) }

  def relationExpr[_: P]: P[Relation] = P( sigmaExpr|originExpr )

  def piExpr[_: P] = P("pi(" ~ funcArguments ~ ")(" ~ relationExpr ~ ")").map { case (attrs, rel) => PiExpr(attrs, rel) }

  def parseAlgebra[_: P]: P[Ast] = P(  (piExpr|relationExpr) ~ End)
}
