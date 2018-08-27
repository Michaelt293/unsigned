package unsigned

import cats._
import cats.implicits._
import org.scalacheck.Arbitrary

import Integer._

final class Unsigned[A: Integer] private (val value: A) {
  def toBigInt: BigInt = value.toBigInt - Integer[A].minValue.toBigInt

  override def toString: String = toBigInt.toString

  override def equals(that: Any): Boolean = that match {
    case unsigned: Unsigned[A] => toBigInt == unsigned.toBigInt
    case _ => false
  }
}

object Unsigned {
  private def applyWithToBigInt[A: Integer, N](n: N)(toBigInt: N => BigInt): Unsigned[A] = {
    assert(toBigInt(n) >= BigInt(0), "Integer cannot be negative")
    assert(toBigInt(n) <= Integer[A].maxValue.toBigInt * 2 + 1, "Integer out of range")
    new Unsigned[A](Integer[A].fromBigInt(toBigInt(n) - Integer[A].minValue.toBigInt))
  }

  def apply[A: Integer, N: Integer](n: N): Unsigned[A] = applyWithToBigInt(n)(_.toBigInt)

  def apply[A: Integer](n: Byte): Unsigned[A] = apply[A, Byte](n)

  def apply[A: Integer](n: Short): Unsigned[A] = apply[A, Short](n)

  def apply[A: Integer](n: Int): Unsigned[A] = apply[A, Int](n)

  def apply[A: Integer](n: Long): Unsigned[A] = apply[A, Long](n)

  def apply[A: Integer](n: BigInt): Unsigned[A] = applyWithToBigInt(n)(identity)

  implicit def unsignedShow[A: Integer : Show]: Show[Unsigned[A]] =
    (x: Unsigned[A]) => x.toString

  implicit def unsignedEq[A: Integer : Eq]: Eq[Unsigned[A]] =
    (x: Unsigned[A], y: Unsigned[A]) => x.toBigInt === y.toBigInt

  implicit def unsignedOrder[A: Integer : Order]: Order[Unsigned[A]] =
    (x: Unsigned[A], y: Unsigned[A]) =>
        Order[BigInt].compare(x.toBigInt, y.toBigInt)

  implicit def unsignedMonoid[A: Integer]: Monoid[Unsigned[A]] = {
    new Monoid[Unsigned[A]] {
      def combine(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] = {
        val x_ = x.toBigInt - Integer[A].minValue.toBigInt
        val y_ = y.toBigInt - Integer[A].minValue.toBigInt
        val result = x.toBigInt + y.toBigInt + Integer[A].minValue.toBigInt
        new Unsigned[A](Integer[A].fromBigInt(result))
      }

      def empty: Unsigned[A] = new Unsigned[A](Integer[A].minValue)
    }
  }

  implicit def unsignedInteger[A: Integer]: Integer[Unsigned[A]] = {
    new Integer[Unsigned[A]] {
      def minValue: Unsigned[A] = Monoid[Unsigned[A]].empty

      def maxValue: Unsigned[A] = new Unsigned[A](Integer[A].minValue)

      def add(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] = x |+| y

      def subtract(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] =
        Unsigned[A](x.toBigInt - y.toBigInt)

      def multiply(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] =
        Unsigned[A](x.toBigInt * y.toBigInt)

      def toBigInt(x: Unsigned[A]): BigInt = x.toBigInt

      def fromBigInt(x: BigInt): Unsigned[A] = apply(x)
    }
  }

  implicit def unsignedArbitrary[A: Arbitrary : Integer]: Arbitrary[Unsigned[A]] =
    Arbitrary(
      Arbitrary.arbitrary[A].map {
        n => new Unsigned[A](n)}
    )
}
