package a_introduction_xpug

import A000_Model._
import A051_HOF_Map._
import A061_HOF_Fold._

object A070_Zip extends Application {

   // bestands differenzen
  
  val bestaende = List( Betrag(100,"EUR"),Betrag(200,"EUR"),Betrag(170,"EUR"),Betrag(300,"EUR"),Betrag(450,"EUR"),Betrag(400,"EUR") )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  def zip[A,B]( lst1 :List[A] )( lst2:List[B] ) : List[(A,B)] =
    
    ( lst1, lst2 ) match {
    
      case ( Nil, _ )           => Nil
      
      case ( _, Nil )           => Nil
      
      case ( a :: as, b :: bs ) => (a,b) :: zip(as)(bs)
    }
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  def pairs[A]( lst :List[A] ) :List[(A,A)] = if( lst.isEmpty ) Nil else zip( lst )( lst.tail )
  
  println( pairs( bestaende ) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  println( map( pairs( bestaende ) )( paar => paar._2 - paar._1 ) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  // stetig wachsender bestand ?
  
  val diffs = ( lst :List[Betrag] ) => map( pairs( lst ) )( paar => paar._2 - paar._1 )
  
  
  val positiveDiffs = ( betraege :List[Betrag] ) => map( diffs( betraege ) )( _ > ZERO_EUR )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  val stetigWachsend = ( betraege :List[Betrag] ) => allTrue( positiveDiffs( betraege ) )
  
  
  
  val bestaende1 = List( Betrag(100,"EUR"),Betrag(200,"EUR"),Betrag(170,"EUR"),Betrag(300,"EUR"),Betrag(450,"EUR"),Betrag(400,"EUR") )
  
  println( "stetig wachsend : " + stetigWachsend( bestaende1 ) )
  
  println( "stetig wachsend : " + stetigWachsend( List( Betrag(100,"EUR"),Betrag(200,"EUR"),Betrag(270,"EUR"),Betrag(300,"EUR")) ) )
}