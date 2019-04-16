package ch.hepia

import ch.hepia.sql.ShowSql._
import ch.hepia.sql.AstShowSql._

import scala.util.Try


object Main {
  import fastparse._

  def main(args: Array[String]): Unit = {

    val toParse: String = Try ( args(0) ).getOrElse("pi(test, coucou)(sigma(t = k)(Person join(p = c) Car join(c = z) Zob))")

    val Parsed.Success(value, successIndex) = parse(toParse, Parser.parseAlgebra(_))

    println("-" * 50)

    println(value.showSql)

    println("-" * 50)

  }


}
