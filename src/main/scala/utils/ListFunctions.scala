package utils

object ListFunctions {

	def zipWith[A,B,C]( zipFunc : A => B => C )( as :List[A] )( bs :List[B] ) :List[C] 
	    =
	    (as, bs) match {
		
		  case ( Nil, _ ) => Nil
		  case ( _, Nil ) => Nil
		  case ( a :: atail, b :: btail ) => zipFunc(a)(b) :: zipWith( zipFunc )( atail )( btail )
	}
	
	// we can treat a list of pairs as a kind of Map and lookup a value for a given key
	def lookup[K,V]( map :List[(K,V)] )( key :K ) :Option[V]  =  map match {
		
		case Nil                       => None
		case ( k,v ) :: _ if k == key  => Some( v )
		case _ :: tail                 => lookup( tail )( key )
	}
	
	
	def map[A,B]( func :A => B ) : List[A] => List[B] =
		
		( as : List[A] ) => as match {
			
			case Nil        => Nil
			
			case a :: atail  => func( a ) :: map( func )( atail )
		}
		
		
	def foldr[A,B]( func : A => B => B )( onEmpty :B )( as :List[A] ) : B  =  as match {
		
		case Nil         => onEmpty
		
		case a :: atail  => func( a )( foldr( func )( onEmpty )( atail ) )
	}
	

}