package unsigned

import cats._
import cats.implicits._
import org.scalacheck.Arbitrary

import Cast._
import ToBigInt._
import FromBigInt._
import MaxBounded._
import MinBounded._

final class Unsigned[A: MinBounded : ToBigInt] private (val value: A) {
  def toBigInt: BigInt =
    value.toBigInt - MinBounded[A].minValue.toBigInt

  override def toString: String = toBigInt.toString

  override def equals(that: Any): Boolean = that match {
    case unsigned: Unsigned[A] => toBigInt == unsigned.toBigInt
    case _ => false
  }
}

object Unsigned {
  private def apply[A: MinBounded : ToBigInt : FromBigInt, N: ToBigInt](n: N): Unsigned[A] = {
    assert(n.toBigInt >= BigInt(0), "Integer cannot be negative")
    assert(n.toBigInt <= MaxBounded[A].maxValue.toBigInt * 2 + 1, "Integer out of range")
    new Unsigned[A]((n.toBigInt - MinBounded[A].minValue.toBigInt).fromBigInt)
  }

  def apply[A: MinBounded : ToBigInt : FromBigInt](n: Byte): Unsigned[A] = apply[A, Byte](n)

  def apply[A: MinBounded : ToBigInt : FromBigInt](n: Short): Unsigned[A] = apply[A, Short](n)

  def apply[A: MinBounded : ToBigInt : FromBigInt](n: Int): Unsigned[A] = apply[A, Int](n)

  def apply[A: MinBounded : ToBigInt : FromBigInt](n: Long): Unsigned[A] = apply[A, Long](n)

  def apply[A: MinBounded : ToBigInt : FromBigInt](n: BigInt): Unsigned[A] = apply[A, BigInt](n)

  implicit def unsignedShow[A: MinBounded : ToBigInt : FromBigInt : Show]: Show[Unsigned[A]] =
    (x: Unsigned[A]) => x.toString

  implicit def unsignedEq[A: MinBounded : ToBigInt : FromBigInt : Eq]: Eq[Unsigned[A]] =
    (x: Unsigned[A], y: Unsigned[A]) => x.toBigInt === y.toBigInt

  implicit def unsignedOrder[A: MinBounded : ToBigInt : FromBigInt : Order]: Order[Unsigned[A]] =
    (x: Unsigned[A], y: Unsigned[A]) =>
        Order[BigInt].compare(x.toBigInt, y.toBigInt)

  implicit def unsignedMonoid[A: MinBounded : ToBigInt : FromBigInt]: Monoid[Unsigned[A]] = {
    new Monoid[Unsigned[A]] {
      def combine(x: Unsigned[A], y: Unsigned[A]): Unsigned[A] = {
        val x_ = x.toBigInt - MinBounded[A].minValue.toBigInt
        val y_ = y.toBigInt - MinBounded[A].minValue.toBigInt
        val result = x.toBigInt + y.toBigInt + MinBounded[A].minValue.toBigInt
        new Unsigned[A](result.fromBigInt)
      }

      def empty: Unsigned[A] = new Unsigned[A](MinBounded[A].minValue)
    }
  }

  implicit def unsignedArbitrary[A: Arbitrary]: Arbitrary[Unsigned[A]] =
    Arbitrary(
      Arbitrary.arbitrary[A].map {
        n => new Unsigned[A](n)}
    )
}
