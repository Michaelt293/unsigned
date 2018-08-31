package unsigned

import cats._
import cats.implicits._
import org.scalacheck.Arbitrary

import unsigned.Integral._

final class Unsigned[A: Bounded : Integral] private(val value: A) {
  def toBigInt: BigInt =
    value.toBigInt - Bounded[A].minValue.toBigInt

  override def toString: String = toBigInt.toString

  override def equals(that: Any): Boolean = that match {
    case unsigned: Unsigned[A] => toBigInt == unsigned.toBigInt
    case _ => false
  }
}

object Unsigned {
  def apply[A: Bounded : Integral, N: Integral](n: N): Unsigned[A] = {
    new Unsigned[A](Integral[A].fromBigInt(n.toBigInt - Bounded[A].minValue.toBigInt))
  }

  def apply[A: Bounded : Integral](n: Byte): Unsigned[A] = apply[A, Byte](n)

  def apply[A: Bounded : Integral](n: Short): Unsigned[A] = apply[A, Short](n)

  def apply[A: Bounded : Integral](n: Int): Unsigned[A] = apply[A, Int](n)

  def apply[A: Bounded : Integral](n: Long): Unsigned[A] = apply[A, Long](n)

  def apply[A: Bounded : Integral](n: BigInt): Unsigned[A] = apply[A, BigInt](n)

  implicit def unsignedShow[A: Bounded : Integral]: Show[Unsigned[A]] =
    (x: Unsigned[A]) => x.toString

  implicit def unsignedEq[A: Bounded : Integral : Eq]: Eq[Unsigned[A]] =
    (x: Unsigned[A], y: Unsigned[A]) => x.toBigInt === y.toBigInt

  implicit def unsignedOrder[A: Bounded : Integral : Order]: Order[Unsigned[A]] =
    (x: Unsigned[A], y: Unsigned[A]) =>
        Order[BigInt].compare(x.toBigInt, y.toBigInt)

  implicit def unsignedMonoid[A: Bounded : Integral]: Monoid[Unsigned[A]] = {
    new Monoid[Unsigned[A]] {
      def combine(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] = {
        val result = x.toBigInt + y.toBigInt + Bounded[A].minValue.toBigInt
        new Unsigned[A](Integral[A].fromBigInt(result))
      }

      def empty: Unsigned[A] = new Unsigned[A](Bounded[A].minValue)
    }
  }

  implicit def unsignedArbitrary[A: Arbitrary : Bounded : Integral]: Arbitrary[Unsigned[A]] =
    Arbitrary(
      Arbitrary.arbitrary[A].map {
        n => new Unsigned[A](n)
      }
    )

  implicit def unsignedBounded[A: Bounded : Integral]: Bounded[Unsigned[A]] =
    Bounded.instance(
      new Unsigned[A](Bounded[A].minValue),
      new Unsigned[A](Bounded[A].maxValue)
    )

  implicit def unsignedIntegral[A: Bounded : Integral]: Integral[Unsigned[A]] =
    new Integral[Unsigned[A]] {
      def toBigInt(n: Unsigned[A]): BigInt = n.toBigInt

      def fromBigInt(n: BigInt): Unsigned[A] = Unsigned(n)

      def add(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] = x |+| y

      def subtract(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] = {
        val result = x.toBigInt - y.toBigInt + Bounded[A].minValue.toBigInt
        new Unsigned[A](Integral[A].fromBigInt(result))
      }

      def multiply(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] = {
        val result = x.toBigInt * y.toBigInt + Bounded[A].minValue.toBigInt
        new Unsigned[A](Integral[A].fromBigInt(result))
      }
    }
}
