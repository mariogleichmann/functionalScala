package typeclassopedia.monoid

object Instances {

  implicit object IntMonoid extends Monoid[Int]{
    def add(x : Int, y : Int): Int = x + y
    def unit : Int = 0
  }

  
  implicit object StringMonoid extends Monoid[String]{
    def add(x : String, y : String): String = x + y
    def unit : String = ""
  }
  
  implicit def optionMonoid[A](implicit aMonoid: Monoid[A]): Monoid[ Option[A] ] =
    new Monoid[ Option[A] ] {
     
      def add( opt1 : Option[A], opt2 : Option[A]): Option[A] = {
        (opt1,opt2) match {          
          case ( None, o )			=> o
          case ( o, None )			=> o
          case ( Some(x), Some(y) )	=> Some( aMonoid.add( x, y ) )	      
	    }      
      }
    
      def unit : Option[A] = None
    }
}
