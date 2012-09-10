package a_introduction_xpug

import A0332_HOF_Filter_Generic._
import A000_Model._
import A0333_HOF_Filter_Auftraege_Generics._

object A034_HOF_PredicateTransformator {

    
   val auftraege = KaufAuftrag( Betrag(100,"EUR") ) :: 
	   			   KaufAuftrag( Betrag(250,"EUR") ) :: 
	   			   VerkaufAuftrag( Betrag(200,"EUR") ) ::
	   			   KaufAuftrag( Betrag(100,"EUR") ) ::
	   			   VerkaufAuftrag( Betrag(70,"EUR") ) ::
	   			   VerkaufAuftrag( Betrag(50,"EUR") ) ::
	   			   Nil
  
  
	   			   
	   			   
  def not[A] ( pred : A => Boolean ) : A => Boolean = 
    
    a => pred( a ) match {
      
      case true => false
      case _    => true
    }
    
    
    
  val noVerkaufAuftraege = filter( auftraege )( not( isKauf ) )

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(30,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )

   
   val negativ = ( betrag :Betrag ) => betrag < ZERO_EUR
   
   
   val positivePrNullBetraege = filter( betraege )( not( negativ ) )
   

  
  
  
}