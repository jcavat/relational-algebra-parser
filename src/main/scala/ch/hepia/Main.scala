package ch.hepia


object Main {
  import fastparse._

  def main(args: Array[String]): Unit = {
    val Parsed.Success(value, successIndex) = parse("pi(test, coucou)(sigma(t = k)(Person join(u = b) Voiture))", Parser.piArguments(_))
    println(value)
  }


}
