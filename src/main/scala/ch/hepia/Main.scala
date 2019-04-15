package ch.hepia


object Main {
  import fastparse._

  def main(args: Array[String]): Unit = {
    //val Parsed.Success(value, successIndex) = parse("pi(test, coucou)(sigma(t = k)(Person join(p = v) Voiture join(v = t) Test join(t = g) Groz))", Parser.parseAlgebra(_))
    //val Parsed.Success(value, successIndex) = parse("pi(test, coucou)(sigma(t = k)(Person))", Parser.parseAlgebra(_))
    val Parsed.Success(value, successIndex) = parse("pi(test, coucou)(sigma(t = k)(Person join(p = c) Car join(c = z) Zob))", Parser.parseAlgebra(_))


    println(value)
    println(ShowSql[Ast].showSql(value))
    // println( value.showSql )
//    println(value.showSql)

  }


}
