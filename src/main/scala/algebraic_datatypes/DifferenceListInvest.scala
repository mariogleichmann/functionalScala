package algebraic_datatypes

object DifferenceListInvest extends Application{
	
  def appendLists[A]( xs :List[A])( ys :List[A]) : List[A]
    =
    xs match {	  
	  case ( a :: as ) 	=> a :: appendLists( as )( ys )
	  case _			=> ys
    }
  

  case class DList[A]( unDl : List[A] => List[A] )
	
  
  def appendDLists[A] : ( DList[A], DList[A] ) => DList[A] 
    =
    ( xs, ys ) => ( xs, ys ) match {  	
    	case( DList( appFunc1 ), DList( appFunc2 ) ) => DList( x => appFunc1( appFunc2( x ) ) ) // just function composition: f( g( x ) )
    }
    
    
  def fromList[A] : List[A] => DList[A]
    =
    xs => DList( appendLists( xs ) )
    
    
  def toList[A] : DList[A] => List[A]
    = _ match {
	  case DList( appFunc ) => appFunc( Nil ) 
    }
  
  
  val dLst :DList[String] = appendDLists( fromList( "a" :: Nil ), fromList( "b" :: Nil ) )
  
  val lst : List[String] = toList( dLst )
  
  println( dLst )
  println( lst )
  
  
  val anotherDlist :DList[Int] = appendDLists( fromList( 1 :: Nil ), fromList( 2 :: Nil ) )
  
  println( anotherDlist )
  println( toList( anotherDlist ) )  
}