package typeclassopedia.eq

trait Eq[E]{
    
  def eq( e1 :E)( e2 :E ) : Boolean
}
