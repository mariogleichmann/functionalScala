package typeclassopedia.ord

import typeclassopedia.eq.Eq

trait Ord[a] extends Eq[a] {
  
  override def eq(a1: a)(a2: a) = compare(a1)(a2) == EQ
  
  def compare(a1: a)(a2: a): Ordering
}