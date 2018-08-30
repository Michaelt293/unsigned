package unsigned

trait Bounded[A] {
  def minValue: A

  def maxValue: A
}

object Bounded {
  def apply[A](implicit b: Bounded[A]): Bounded[A] = b

  implicit def instance[A](min: A, max: A): Bounded[A] = new Bounded[A] {
    def minValue: A = min

    def maxValue: A = max
  }

  implicit val byteBounded: Bounded[Byte] = instance(Byte.MinValue, Byte.MaxValue)

  implicit val shortBounded: Bounded[Short] = instance(Short.MinValue, Short.MaxValue)

  implicit val intBounded: Bounded[Int] = instance(Int.MinValue, Int.MaxValue)

  implicit val longBounded: Bounded[Long] = instance(Long.MinValue, Long.MaxValue)
}
