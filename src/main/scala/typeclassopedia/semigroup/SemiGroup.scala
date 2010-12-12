package typeclassopedia.semigroup

trait SemiGroup[S] {
    
  def add(x :S, y :S): S
}
