package typeclassopedia.monoid

import typeclassopedia.semigroup.SemiGroup

trait Monoid [M] extends SemiGroup[M] {
    
  def unit : M
}
