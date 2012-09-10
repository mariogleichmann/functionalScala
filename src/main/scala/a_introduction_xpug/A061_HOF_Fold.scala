package a_introduction_xpug

import A060_HOF_Fold._
import A051_HOF_Map._
import A000_Model._

object A061_HOF_Fold extends Application{


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  val isum = fold( 0 )( ( _:Int) + (_:Int ) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  val prod = fold( 1 )( (_:Int) * (_:Int) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  val allTrue = fold( true )( (_:Boolean) && (_:Boolean)  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  val concat = fold( "" )( (_:String) + (_:String) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  def append[A]( lst1 :List[A] )( lst2 :List[A] ) = fold( lst2 )( ( x:A, lst:List[A] ) => x :: lst )( lst1 )
    
  println( append( 1::2::3::Nil )( 4::5::6::Nil ) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  val listLength =  (lst :List[_]) => isum( map( lst )( _ => 1 ) )
  
  println( listLength( "a"::1::true::2::5::false::"m"::Nil ) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  def appendIf[A]( pred : A => Boolean ) : ( A, List[A] ) => List[A] 
  	=
    ( a :A, acc :List[A] ) => if( pred( a ) ) a :: acc else acc

    
  def filterByFold[A]( pred :A => Boolean ) = fold( Nil :List[A] )( appendIf( pred ) )
  
  
  
  

  val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(30,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )
  
  println(  filterByFold( ( betrag :Betrag) => betrag > ZERO_EUR )( betraege ) )
  
  
}