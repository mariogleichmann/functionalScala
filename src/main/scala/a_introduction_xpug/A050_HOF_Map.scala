package a_introduction_xpug

import A000_Model._
import A0332_HOF_Filter_Generic._
import A0333_HOF_Filter_Auftraege_Generics._

object A050_HOF_Map extends Application{

  
    val sum : List[Betrag] => Betrag  
        =  
        betraege => betraege match {
      
          case Nil 	    => ZERO_EUR
          
          case b :: bs  => b + sum( bs )
        }
  
        
    val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(30,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )
  
    println( "sum : " + sum( betraege ) )
    
    
    

   val auftraege = KaufAuftrag( Betrag(100,"EUR") ) :: 
   			       KaufAuftrag( Betrag(250,"EUR") ) :: 
   			       VerkaufAuftrag( Betrag(200,"EUR") ) ::
   			       KaufAuftrag( Betrag(100,"EUR") ) ::
   			       VerkaufAuftrag( Betrag(70,"EUR") ) ::
   			       VerkaufAuftrag( Betrag(50,"EUR") ) ::
   			       Nil
   			       
  // ... ???   			       
}