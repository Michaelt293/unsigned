package unsigned

import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite

class TreeLawTests extends CatsSuite {
  import Unsigned._
  checkAll("Unsigned[Byte].MonoidTests", MonoidTests[Unsigned[Byte]].monoid)
  checkAll("Unsigned[Short].MonoidTests", MonoidTests[Unsigned[Short]].monoid)
  checkAll("Unsigned[Int].MonoidTests", MonoidTests[Unsigned[Int]].monoid)
  checkAll("Unsigned[Long].MonoidTests", MonoidTests[Unsigned[Long]].monoid)
}
