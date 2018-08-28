package unsigned

trait Cast[A, B] {
  def cast(from: A): B
}

object Cast {
  type ToBigInt[A] = Cast[A, BigInt]

  type FromBigInt[A] = Cast[BigInt, A]

  def apply[A, B](implicit c: Cast[A, B]): Cast[A, B] = c

  def instance[A, B](f: A => B): Cast[A, B] =
    (from: A) => f(from)

  implicit def identityCast[A](f: A => A): Cast[A, A] = (a: A) => identity(a)

  implicit val byteToBigInt: Cast[Byte, BigInt] = instance((x: Byte) => BigInt(x))

  implicit val bigIntToByte: Cast[BigInt, Byte] = instance((x: BigInt) => x.byteValue)

  implicit val shortToBigInt: Cast[Short, BigInt] = instance((x: Short) => BigInt(x))

  implicit val bigIntToShort: Cast[BigInt, Short] = instance((x: BigInt) => x.shortValue)

  implicit val intToBigInt: Cast[Int, BigInt] = instance((x: Int) => BigInt(x))

  implicit val bigIntoToInt: Cast[BigInt, Int] = instance((x: BigInt) => x.intValue)

  implicit val longToBigInt: Cast[Long, BigInt] = instance((x: Long) => BigInt(x))

  implicit val bigIntToLong: Cast[BigInt, Long] = instance((x: BigInt) => x.longValue)

//  implicit class RichCast[A: Cast, B: Cast](value: A) {
//      def cast: B = Cast[A, B].cast(value)
//    }
}

object ToBigInt {
  import Cast._

  def apply[A](implicit t: ToBigInt[A]): ToBigInt[A] = t

  implicit class RichToBigInt[A: ToBigInt](value: A) {
    def toBigInt: BigInt = ToBigInt[A].cast(value)
  }
}

object FromBigInt {
  import Cast._

  def apply[A](implicit t: FromBigInt[A]): FromBigInt[A] = t

  implicit class RichToBigInt(value: BigInt) {
    def fromBigInt[A: FromBigInt]: A = FromBigInt[A].cast(value)
  }
}
