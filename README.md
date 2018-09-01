# Unsigned

Scala has 8, 16, 32 and 64 bit signed integers (`Byte`, `Short`, `Int` and `Long`, respectively).
In addition, Scala also has an arbitrary-precision signed integer type, `BigInt`. Interestingly,
Scala does not have unsigned integer types. In typed programming, the aim is to make illegal
states under representable. Unsigned integers can be very useful in achieving this goal. For example,
a negative count or age does not make sense. Therefore, it would be better to represent a count or
age with an unsigned integer type than a signed integer type. In the example below, an value class
`Age` is defined wrapping an `Int`. This allows a negative age to be constructed. (It should be noted
that smart constructors could be used to guard against this issue. This approach, however, introduces
the issue of having to deal with `Option` or `Either` types.)

```scala
scala> case class Age(age: Int) extends AnyVal
defined class Age

scala> Age(-9)
res0: Age = Age(-9)
```

This project contains the data type, `Unsigned`. `Unsigned` wraps a signed integer and allows us
to use the integer as if it was unsigned. Values of unsigned may be constructed from any unsigned
integer type. Care should be taken though to avoid integer underflow or overflow.

```scala
scala> import unsigned.Unsigned
import unsigned.Unsigned

scala> import unsigned.Unsigned._
import unsigned.Unsigned._

scala> Unsigned[Byte](6)
res1: unsigned.Unsigned[Byte] = 6

scala> Unsigned[Byte](-1)
res2: unsigned.Unsigned[Byte] = 255

scala> Unsigned[Byte](256)
res3: unsigned.Unsigned[Byte] = 0
```

Instances for Cats `Eq`, `Ord`, `Monoid` and `Show` type classes are provided for `Unsigned`.

```scala
scala> import cats._
import cats._

scala> import cats.implicits._
import cats.implicits._

scala> List(1,2,3).foldMap(Unsigned[Long]) > Unsigned[Long](10)
res4: Boolean = false
```

A more complete example using `Unsigned` is provided below. In this example, the total count
for each word in a block of text is calculated together with the maximum count of each word
in a sentence.

```scala
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
```

Results -

```scala
scala> stats.filter { case (word, _)  => Set("scala", "haskell", "functional", "programming")(word) }
res1: Map[String,Stats] =
  Map(
    functional -> Stats(Count(3),Count(1)),
    programming -> Stats(Count(4),Count(2)),
    scala -> Stats(Count(2),Count(1)),
    haskell -> Stats(Count(2),Count(1)))
```