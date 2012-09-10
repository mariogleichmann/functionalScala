package algebraic_datatypes

object ListDemo extends Application{

	// if we wouldn't allow covariance (using + before type parameter T) ...
	sealed abstract case class ADTList[+T]{
		//def head : T
		def :-:[S >: T] ( x :S ) :ADTList[S] = ADTCons( x, this ) // adhering covariance in T, we need x to be a supertype of T (or T)
		def --:[S >: T] ( x :S ) :ADTList[S] = ADTCons( x, this )
	}
	case object ADTNil extends ADTList[Nothing]{
		//def head : Nothing = error( "no element" ) // error is a method which results into something of type Nothing!!!
	}
	case class ADTCons[T]( x :T, tail :ADTList[T] ) extends ADTList[T]{
		//def head = x
	}
	
	// ... we couldn't assign ADTNil (of type ADTList[Nothing]) as a subtype of  ADTList[Int]
	// (and that's all only possible because type Nothing is the subtype of all types
	val emptyInts : ADTList[Int] = ADTNil
	
	val someInts : ADTList[Int] = ADTCons( 2, ADTCons( 1, ADTNil ) )
	

	val emptyBools : ADTList[Boolean] = ADTNil
	
	val someBools = true :-: true :-: false :-: true :-: ADTNil
	
	val sb : ADTList[Boolean] = false --: true --: ADTNil
	
	println( size( someBools ) )

	
	
	def empty[T]( list : ADTList[T] ) = list eq ADTNil // eq works nicely, because ADTNil is a singleton object
	//def size[T]( list : ADTList[T] ) = if( empty( list ) ) 0 else size(  )   <-- we need pattern matching
	def size[T]( list : ADTList[T] ) : Int = list match {
		case ADTNil => 0
		case ( x ADTCons xs ) => 1 + size( xs )
	}
	
	def append[A]( xs :ADTList[A], ys :ADTList[A] ) : ADTList[A] = xs match {
		
		case ADTNil			=> ys
		case ( z ADTCons zs ) => ADTCons( z, append( zs, ys ) )
	}
		
	def sublists[T]( list : ADTList[T] ) : ADTList[ADTList[T]] = list match {
		case ADTNil 			=> ADTCons( ADTNil, ADTNil )
		//case ( x ADTCons xs )	=> append( sublists( xs ), for( ys <- sublists( xs ) ) yield ( x :: ys ) ) 
		case ( x ADTCons xs )	=> append( sublists( xs ), foreach( sublists( xs ), (ys :ADTList[T]) => ADTCons( x, ys ) ) )
	}
	
	def foreach[T,R]( xs :ADTList[T], f: T => R ) : ADTList[R] = xs match{
		case ADTNil 			=> ADTNil
		case ( x ADTCons xs )	=> ADTCons( f( x ), foreach( xs, f ) )
	}
	
	

	val someOtherInts = 1 :-: 2 :-: 3 :-: 4 :-: ADTNil
	
	val sublistsInt = sublists( someOtherInts )
	
	foreach( sublistsInt, 
			(ys :ADTList[Int]) => {  
									foreach( ys, 
									( x :Int ) => print( x + " " ) ) 
									println( " " )
									} )

}