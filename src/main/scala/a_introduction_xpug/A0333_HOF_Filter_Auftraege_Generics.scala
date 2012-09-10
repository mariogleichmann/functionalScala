package a_introduction_xpug

import A000_Model._
import A0332_HOF_Filter_Generic._

object A0333_HOF_Filter_Auftraege_Generics extends Application{

  
   val auftraege = KaufAuftrag( Betrag(100,"EUR") ) :: 
	   			   KaufAuftrag( Betrag(250,"EUR") ) :: 
	   			   VerkaufAuftrag( Betrag(200,"EUR") ) ::
	   			   KaufAuftrag( Betrag(100,"EUR") ) ::
	   			   VerkaufAuftrag( Betrag(70,"EUR") ) ::
	   			   VerkaufAuftrag( Betrag(50,"EUR") ) ::
	   			   Nil
  
	
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
	   			   
  val isKauf = ( auftrag :Auftrag ) => auftrag match {
    
    case KaufAuftrag( _ ) => true
    case _                => false
  }
  
  
  
  val kaufAuftraege = filter( auftraege )( isKauf )
  
  
  println( kaufAuftraege )
}