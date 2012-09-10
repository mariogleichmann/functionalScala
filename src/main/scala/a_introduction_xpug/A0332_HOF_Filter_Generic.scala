package a_introduction_xpug

import A000_Model._

object A0332_HOF_Filter_Generic {

	def filter[A]( list :List[A] ) : (A => Boolean) => List[A] =   
	  
	  pred =>
	  
	  	list match {
	    
	  		case Nil 	 				 => Nil
	    
	  		case a :: as if( pred( a ) ) => a :: filter( as )( pred )
	    
	  		case a :: as				 => filter( as )( pred )
	  	}

	  	

   val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(30,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )

   val filterBetraege = filter( betraege )
   
   filterBetraege( _ < ZERO_EUR )
   
 
	   			   
}