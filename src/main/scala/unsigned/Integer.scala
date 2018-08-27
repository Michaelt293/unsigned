package unsigned

trait Integer[A] {
  def minValue: A
  def maxValue: A
  def add(x: A, y: A): A
  def subtract(x: A, y: A): A
  def multiply(x: A, y: A): A
  def toBigInt(x: A): BigInt
  def fromBigInt(x: BigInt): A
}

object Integer {
  def apply[A](implicit i: Integer[A]): Integer[A] = i

  implicit val byteInteger: Integer[Byte] = new Integer[Byte] {
    def minValue: Byte = Byte.MinValue

    def maxValue: Byte = Byte.MaxValue

    def add(x: Byte, y: Byte): Byte = (x + y).toByte

    def subtract(x: Byte, y: Byte): Byte = (x - y).toByte

    def multiply(x: Byte, y: Byte): Byte = (x * y).toByte

    def toBigInt(x: Byte): BigInt = BigInt(x)

    def fromBigInt(x: BigInt): Byte = x.byteValue
  }

  implicit val shortInteger: Integer[Short] = new Integer[Short] {
    def minValue: Short = Short.MinValue

    def maxValue: Short = Short.MaxValue

    def add(x: Short, y: Short): Short = (x + y).toShort

    def subtract(x: Short, y: Short): Short = (x - y).toShort

    def multiply(x: Short, y: Short): Short = (x * y).toShort

    def toBigInt(x: Short): BigInt = BigInt(x)

    def fromBigInt(x: BigInt): Short = x.shortValue
  }

  implicit val intInteger: Integer[Int] = new Integer[Int] {
    def minValue: Int = Int.MinValue

    def maxValue: Int = Int.MaxValue

    def add(x: Int, y: Int): Int = x + y

    def subtract(x: Int, y: Int): Int = x - y

    def multiply(x: Int, y: Int): Int = x * y

    def toBigInt(x: Int): BigInt = BigInt(x)

    def fromBigInt(x: BigInt): Int = x.intValue
  }

  implicit val longInteger: Integer[Long] = new Integer[Long] {
    def minValue: Long = Long.MinValue

    def maxValue: Long = Long.MaxValue

    def add(x: Long, y: Long): Long = x + y

    def subtract(x: Long, y: Long): Long = x - y

    def multiply(x: Long, y: Long): Long = x - y

    def toBigInt(x: Long): BigInt = BigInt(x)

    def fromBigInt(x: BigInt): Long = x.longValue
  }

  implicit class RichInteger[A: Integer](value: A) {
    def +(that: A): A = Integer[A].add(value, that)

    def -(that: A): A = Integer[A].subtract(value, that)

    def *(that: A): A = Integer[A].multiply(value, that)

    def toBigInt: BigInt = Integer[A].toBigInt(value)
  }
}