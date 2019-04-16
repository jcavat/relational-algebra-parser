package ch.hepia.sql

trait ShowSql[A] {
  def showSql( a: A ): String
}

object ShowSql {
  def apply[A]( implicit s: ShowSql[A] ) = s
  def showSql[A: ShowSql](a: A) = ShowSql[A].showSql(a)
  implicit class ShowOps[A: ShowSql](a: A) {
    def showSql = ShowSql[A].showSql(a)
  }
}

