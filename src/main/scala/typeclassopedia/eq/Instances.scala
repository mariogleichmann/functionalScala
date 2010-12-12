package typeclassopedia.eq

object instances {

  implicit def optionEq[A](implicit aEq: Eq[A]): Eq[ Option[A] ] =
    new Eq[ Option[A] ] {
	  def eq( o1 :Option[A])( o2 :Option[A] ) : Boolean = {
	    (o1,o2) match {
          case ( Some(x), Some(y) )	=> aEq.eq( x )( y )
          case ( None, None )		=> true
          case _					=> false	      
	    }
	  }
    }
  
  
  implicit def pairEq[A,B](implicit aEq: Eq[A], bEq: Eq[B]): Eq[ (A,B) ] =
    new Eq[ (A,B) ] {  
      def eq(t1: (A,B))( t2: (A,B)) =
        aEq.eq( t1._1)( t2._1) && bEq.eq( t1._2)( t2._2)
    }

  
  implicit object IntEq extends Eq[Int]{
    def eq( i1 :Int)( i2 :Int ) : Boolean = i1 == i2
  }
  
  
  implicit object StringEq extends Eq[String]{
    def eq( s1 :String)( s2 :String ) : Boolean = s1 == s2
  }
  
}
