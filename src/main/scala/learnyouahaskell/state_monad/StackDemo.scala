package learnyouahaskell.state_monad

object StackDemo extends Application{

	type Stack = List[Int]
	
	val pop : Stack => ( Int, Stack )  
		=  
		_ match {
			case (x :: xs)  => (x, xs )
			case Nil		=> error( "empty stack" )
	}
	
	val push : Int => Stack => ( Unit, Stack )  
		=  
		( x :Int ) => ( xs :Stack ) => ( (), x::xs )
	
	//
	val stackManip : Stack => ( Int, Stack )  
		=  
		( stack :Stack ) => {		
			val ( (), newStack1 ) = push( 3 )( stack )
			val ( a, newStack2 ) = pop( newStack1 )
			pop( newStack2 )
	}
	
	println( stackManip( List(5,8,2,1) ) )  // >>  ( 5, List( 8, 2, 1 ) )
	
}