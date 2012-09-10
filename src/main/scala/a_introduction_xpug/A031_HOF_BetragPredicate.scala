package a_introduction_xpug

import A000_Model._

object A031_HOF_BetragPredicate extends Application{

  
  val filter : ( Betrag => Boolean, List[Betrag] ) => List[Betrag]
  	  = 
  	  ( predicate, betraege ) => betraege match {
  	    
  	     case Nil 							=> Nil
  	     
  	     case b :: bs   => if( predicate( b ) ) b :: filter( predicate, bs ) 
  	     					else filter( predicate, bs )
  	     //case b :: bs						=> filter( predicate, bs )
  	    

  	  } 
    
    		  		
    		
   val positiv = ( betrag :Betrag ) => betrag > ZERO_EUR
   
   val gerade = ( betrag :Betrag ) => betrag.value % 2 == 0
   
   
   
   val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(30,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )
   
   val liste = 1 :: 2 :: 3 :: 4 :: Nil
   
   val positiveBetraege = filter( positiv, betraege )
   
   val geradeBetraege = filter( gerade, betraege )
     
   
   println( positiveBetraege )
   
   println( geradeBetraege )
}