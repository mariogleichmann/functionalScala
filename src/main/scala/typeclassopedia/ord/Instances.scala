package typeclassopedia.ord

object Instances {

  implicit object IntAscOrd extends Ord[Int]{
    def compare(a1: Int)(a2: Int): Ordering = if( a1 > a2 ) GT else if( a1 < a2 ) LT else EQ
  }
  
    
  implicit object IntDescOrd extends Ord[Int]{
    def compare(a1: Int)(a2: Int): Ordering = if( a1 < a2 ) GT else if( a1 > a2 ) LT else EQ
  }
}