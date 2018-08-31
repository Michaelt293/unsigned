package unsigned

import cats._
import cats.implicits._

import unsigned.Unsigned._

object Example {
  case class Count(count: Unsigned[Int])

  object Count {
    def apply(count: Long): Count = Count(Unsigned[Int](count))

    implicit def countOrder: Order[Count] =
      (x: Count, y: Count) =>
        Order[Unsigned[Int]].compare(x.count, y.count)

    implicit val countMonoid: Monoid[Count] = new Monoid[Count] {
      def combine(x: Count, y: Count): Count =
        Count(x.count |+| y.count)

      def empty: Count = Count(0)
    }
  }

  case class Stats(total: Count, maxCount: Count)

  object Stats {
    implicit val statsMonoid: Monoid[Stats] = new Monoid[Stats] {
      def combine(x: Stats, y: Stats): Stats =
        Stats(x.total |+| y.total, x.maxCount.max(y.maxCount))

      def empty: Stats = Stats(Count(0), Count(0))
    }
  }

  val text: String = """
Scala is a programming language that embraces both object orientated and
functional programming paradigms. Functional programming has been around
for many years, however, in recent years it has been become increasingly
popular. The most influential pure functional programming language is
arguably Haskell. The Scala libraries, Scalaz and Cats, have been heavily
influenced by Haskell.
"""

  val sentences: List[String] = text.split('.').toList

  def wordCount(sentence: String): Map[String, Count] =
    sentence
      .toLowerCase
      .split("\\W+")
      .foldLeft(Map.empty[String, Count])((acc, word) => acc |+| Map(word -> Count(1)))

  val wordCounts: List[Map[String, Count]] = sentences.map(wordCount)

  val stats: Map[String, Stats] =
    wordCounts.foldMap(_.mapValues(count => Stats(count, count)))
}
