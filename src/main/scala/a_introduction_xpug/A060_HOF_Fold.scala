package a_introduction_xpug

import A000_Model._
import A0332_HOF_Filter_Generic._
import A0333_HOF_Filter_Auftraege_Generics._

object A060_HOF_Fold extends Application{

  def fold[A,B]( onEmpty :B ) : ( (A,B) => B ) => List[A] => B  =  
    
    func => elems => {    
    
      elems match {
    	  
        case Nil        => onEmpty
        
        case head::tail => func( head , fold( onEmpty )( func )( tail ) ) 
      }
    }
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  val sumBetraege = fold( ZERO_EUR )( (a :Betrag, b :Betrag ) => a + b )
  
  
  val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(30,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )
  

  println( "sum : " + sumBetraege( betraege ) )  
   
   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   val auftraege = KaufAuftrag( Betrag(100,"EUR") ) :: 
   			       KaufAuftrag( Betrag(250,"EUR") ) :: 
   			       VerkaufAuftrag( Betrag(200,"EUR") ) ::
   			       KaufAuftrag( Betrag(100,"EUR") ) ::
   			       VerkaufAuftrag( Betrag(70,"EUR") ) ::
   			       VerkaufAuftrag( Betrag(50,"EUR") ) ::
   			       Nil
	   			
   			       
   			       
  val sumAuftraege = fold( ZERO_EUR )( ( a :Auftrag, sum :Betrag ) => a.betrag + sum )
  
  
  println( "sum : " + sumAuftraege( auftraege ) )  
  
  println( "sum : " + sumAuftraege( filter( auftraege )( isKauf ) ) )  
  
  
}