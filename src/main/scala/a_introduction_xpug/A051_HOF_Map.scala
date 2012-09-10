package a_introduction_xpug


import A000_Model._
import A0332_HOF_Filter_Generic._
import A0333_HOF_Filter_Auftraege_Generics._
import A050_HOF_Map._

object A051_HOF_Map extends Application{

  
  def map[A,B]( list :List[A] ) : ( A => B ) => List[B]  
      =   
	 f => list match {
	   
	     case Nil      =>  Nil
	   
	     case a :: as  =>  f( a ) :: map( as )( f )
     }

         
         
   val auftraege = KaufAuftrag( Betrag(100,"EUR") ) :: 
   			       KaufAuftrag( Betrag(250,"EUR") ) :: 
   			       VerkaufAuftrag( Betrag(200,"EUR") ) ::
   			       KaufAuftrag( Betrag(100,"EUR") ) ::
   			       VerkaufAuftrag( Betrag(70,"EUR") ) ::
   			       VerkaufAuftrag( Betrag(50,"EUR") ) ::
   			       Nil
         
      
     println( "sum all : " + sum( map( auftraege )( auftrag => auftrag.betrag ) ) )

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
     println( "sum invest : " + sum( map( filter( auftraege )( isKauf ) ) ( _.betrag )  ) )
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     val effectiveBetrag = ( auftrag : Auftrag ) => if( isKauf( auftrag ) ) auftrag.betrag else -auftrag.betrag
     
     println( "sum saldo : " + sum( map( auftraege ) ( effectiveBetrag ) ) )
     
     
     
     
     
     
     
}