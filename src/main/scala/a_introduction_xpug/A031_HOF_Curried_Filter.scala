package a_introduction_xpug

import A030_HigherOrderFunc_FilterEvenValues._

object A031_HOF_Curried_Filter {

    val filter : ( Betrag => Boolean ) =>  List[Betrag] => List[Betrag] 
        =  
        predicate => 
          
          betraege => betraege match {
  	    
  	     case Nil 							=> Nil
  	     
  	     case b :: bs if( predicate( b ) )  => b :: filter( predicate )( bs )
  	    
  	     case b :: bs						=> filter( predicate )( bs )
  	    
  	  } 
    
    
   val filterPositive :List[Betrag] => List[Betrag]
      =
      filter( betrag => betrag > ZERO_EUR )
   
   
   val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(31,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )
   
   val moreBetraege = List( Betrag(100,"EUR"),Betrag(-211,"EUR"),Betrag(0,"EUR"),Betrag(311,"EUR"),Betrag(455,"EUR"),Betrag(-488,"EUR") )
   
   
   val positive = filterPositive( betraege )
   
   val morePositive = filterPositive( betraege )
   
   
   println( positive )
   
   println( morePositive )
   
}