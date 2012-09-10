package a_introduction_xpug

import A000_Model._

object A031_HOF_BetragPredicate2 extends Application{

  
  val filter : ( Betrag => Boolean, List[Betrag] ) => List[Betrag]
  	  = 
  	  ( predicate, betraege ) => betraege match {
  	    
  	     case Nil 							=> Nil
  	     
  	     case b :: bs if( predicate( b ) )  => b :: filter( predicate, bs )
  	    
  	     case b :: bs						=> filter( predicate, bs )
  	    
  	  } 
    		
   
   val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(30,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )
   
     
   val ungeradeBetraege = filter( betrag => betrag.value % 2 != 0, betraege )
   
   val negativeBetraege = filter( _ < ZERO_EUR, betraege ) 
   
   
}