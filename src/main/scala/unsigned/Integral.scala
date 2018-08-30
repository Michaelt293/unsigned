package unsigned

trait Integral[A] {
  def toBigInt(n: A): BigInt

  def fromBigInt(n: BigInt): A

  def add(x: A, y: A): A

  def subtract(x: A, y: A): A

  def multiply(x: A, y: A): A
}

object Integral {
  def apply[A](implicit t: Integral[A]): Integral[A] = t

  implicit def instance[A](to: A => BigInt,
                           from: BigInt => A,
                           add: (A, A) => A,
                           subtract: (A, A) => A,
                           mult: (A, A) => A): Integral[A] = {
    new Integral[A] {
      def toBigInt(n: A): BigInt = to(n)

      def fromBigInt(n: BigInt): A = from(n)

      def add(x: A, y: A): A = add(x, y)

      def subtract(x: A, y: A): A = subtract(x, y)

      def multiply(x: A, y: A): A = mult(x, y)
    }
  }

  implicit val fromBigIntByte: Integral[Byte] =
    instance(
      (x: Byte) => BigInt(x),
      (x: BigInt) => x.byteValue,
      (x: Byte, y: Byte) => (BigInt(x) + BigInt(y)).byteValue,
      (x: Byte, y: Byte) => (BigInt(x) - BigInt(y)).byteValue,
      (x: Byte, y: Byte) => (BigInt(x) * BigInt(y)).byteValue
    )

  implicit val fromBigIntShort: Integral[Short] =
    instance(
      (x: Short) => BigInt(x),
      (x: BigInt) => x.shortValue,
      (x: Short, y: Short) => (BigInt(x) + BigInt(y)).shortValue,
      (x: Short, y: Short) => (BigInt(x) - BigInt(y)).shortValue,
      (x: Short, y: Short) => (BigInt(x) * BigInt(y)).shortValue
    )

  implicit val fromBigIntInt: Integral[Int] =
    instance(
      (x: Int) => BigInt(x),
      (x: BigInt) => x.intValue,
      (x: Int, y: Int) => x + y,
      (x: Int, y: Int) => x - y,
      (x: Int, y: Int) => x * y
    )

  implicit val fromBigIntLong: Integral[Long] =
    instance(
      (x: Long) => BigInt(x),
      (x: BigInt) => x.longValue,
      (x: Long, y: Long) => x + y,
      (x: Long, y: Long) => x - y,
      (x: Long, y: Long) => x * y
    )

  implicit val fromBigIntBigInt: Integral[BigInt] =
    instance(
      identity,
      identity,
      (x: BigInt, y: BigInt) => x + y,
      (x: BigInt, y: BigInt) => x - y,
      (x: BigInt, y: BigInt) => x * y
    )

  implicit class RichIntegral[A: Integral](value: A) {
    def toBigInt: BigInt = Integral[A].toBigInt(value)

    def +(that: A): A = Integral[A].add(value, that)

    def -(that: A): A = Integral[A].subtract(value, that)

    def *(that: A): A = Integral[A].multiply(value, that)
  }
}
