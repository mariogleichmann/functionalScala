package typeclassopedia.ord

  sealed abstract class Ordering
  final case object LT extends Ordering
  final case object EQ extends Ordering
  final case object GT extends Ordering