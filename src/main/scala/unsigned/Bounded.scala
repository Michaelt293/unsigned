package unsigned

trait MinBounded[A] {
  def minValue: A
}

object MinBounded {
  def apply[A](implicit m: MinBounded[A]): MinBounded[A] = m

  def instance[A](a: A): MinBounded[A] = new MinBounded[A] {
    def minValue: A = a
  }

  implicit val byteMinBounded: MinBounded[Byte] = instance(Byte.MinValue)

  implicit val shortMinBounded: MinBounded[Short] = instance(Short.MinValue)

  implicit val intMinBounded: MinBounded[Int] = instance(Int.MinValue)

  implicit val longMinBounded: MinBounded[Long] = instance(Long.MinValue)
}

trait MaxBounded[A] {
  def maxValue: A
}

object MaxBounded {
  def apply[A](implicit m: MaxBounded[A]): MaxBounded[A] = m

  def instance[A](a: A): MaxBounded[A] = new MaxBounded[A] {
    def maxValue: A = a
  }

  implicit val byteMinBounded: MaxBounded[Byte] = instance(Byte.MaxValue)

  implicit val shortMinBounded: MaxBounded[Short] = instance(Short.MaxValue)

  implicit val intMinBounded: MaxBounded[Int] = instance(Int.MaxValue)

  implicit val longMinBounded: MaxBounded[Long] = instance(Long.MaxValue)
}

trait Bounded[A] extends MinBounded[A] with MaxBounded[A]

object Bounded {
  def apply[A](implicit b: Bounded[A]): Bounded[A] = b
}

//
//object Integer {
//  def apply[A](implicit i: Integer[A]): Integer[A] = i
//
//  implicit val byteInteger: Integer[Byte] = new Integer[Byte] {
//    def minValue: Byte = Byte.MinValue
//
//    def maxValue: Byte = Byte.MaxValue
//
//    def add(x: Byte, y: Byte): Byte = (x + y).toByte
//
//    def subtract(x: Byte, y: Byte): Byte = (x - y).toByte
//
//    def multiply(x: Byte, y: Byte): Byte = (x * y).toByte
//
//    def toBigInt(x: Byte): BigInt = BigInt(x)
//
//    def fromBigInt(x: BigInt): Byte = x.byteValue
//  }
//
//  implicit val shortInteger: Integer[Short] = new Integer[Short] {
//    def minValue: Short = Short.MinValue
//
//    def maxValue: Short = Short.MaxValue
//
//    def add(x: Short, y: Short): Short = (x + y).toShort
//
//    def subtract(x: Short, y: Short): Short = (x - y).toShort
//
//    def multiply(x: Short, y: Short): Short = (x * y).toShort
//
//    def toBigInt(x: Short): BigInt = BigInt(x)
//
//    def fromBigInt(x: BigInt): Short = x.shortValue
//  }
//
//  implicit val intInteger: Integer[Int] = new Integer[Int] {
//    def minValue: Int = Int.MinValue
//
//    def maxValue: Int = Int.MaxValue
//
//    def add(x: Int, y: Int): Int = x + y
//
//    def subtract(x: Int, y: Int): Int = x - y
//
//    def multiply(x: Int, y: Int): Int = x * y
//
//    def toBigInt(x: Int): BigInt = BigInt(x)
//
//    def fromBigInt(x: BigInt): Int = x.intValue
//  }
//
//  implicit val longInteger: Integer[Long] = new Integer[Long] {
//    def minValue: Long = Long.MinValue
//
//    def maxValue: Long = Long.MaxValue
//
//    def add(x: Long, y: Long): Long = x + y
//
//    def subtract(x: Long, y: Long): Long = x - y
//
//    def multiply(x: Long, y: Long): Long = x - y
//
//    def toBigInt(x: Long): BigInt = BigInt(x)
//
//    def fromBigInt(x: BigInt): Long = x.longValue
//  }
//
//  implicit class RichInteger[A: Integer](value: A) {
//    def +(that: A): A = Integer[A].add(value, that)
//
//    def -(that: A): A = Integer[A].subtract(value, that)
//
//    def *(that: A): A = Integer[A].multiply(value, that)
//
//    def toBigInt: BigInt = Integer[A].toBigInt(value)
//  }
//}