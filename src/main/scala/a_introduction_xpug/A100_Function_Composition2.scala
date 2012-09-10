package a_introduction_xpug

import A100_Function_Composition._
import A070_Zip._
import A000_Model._
import A050_HOF_Map._
import A051_HOF_Map._
import A061_HOF_Fold._
import A0332_HOF_Filter_Generic._
import A0333_HOF_Filter_Auftraege_Generics._

object A100_Function_Composition2 {

  
  // stetigWachsend mit func composition
  
  def flip[A,B,C]( f : A => B => C ) : B => A => C = b => a => f(a)(b)
  
  
  val greaterZero = ( b :Betrag ) => b > ZERO_EUR
  
  val checkBetrag = flip( map[Betrag,Boolean] )
  
  val stetigWachsend = allTrue o checkBetrag( greaterZero ) o diffs
  
  
  val bestaende = List( Betrag(100,"EUR"),Betrag(200,"EUR"),Betrag(170,"EUR"),Betrag(300,"EUR"),Betrag(450,"EUR"),Betrag(400,"EUR") )

  println( stetigWachsend( bestaende ) )  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    val auftraege = KaufAuftrag( Betrag(100,"EUR") ) :: 
	   			    KaufAuftrag( Betrag(250,"EUR") ) :: 
	   			    VerkaufAuftrag( Betrag(200,"EUR") ) ::
	   			    KaufAuftrag( Betrag(100,"EUR") ) ::
	   			    VerkaufAuftrag( Betrag(70,"EUR") ) ::
	   			    VerkaufAuftrag( Betrag(50,"EUR") ) ::
	   			    Nil 
  
  
   
   
   val filterKauf :List[Auftrag] => List[Auftrag] =  flip( filter[Auftrag] )( isKauf )
   
   val toBetrag :List[Auftrag] => List[Betrag] = flip( map[Auftrag,Betrag] )( _.betrag )
   
   val invest = sum o toBetrag o filterKauf

   
   println( invest( auftraege ) )
   
}