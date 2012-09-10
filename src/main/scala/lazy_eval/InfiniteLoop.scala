package lazy_eval

object InfiniteLoop extends Application{

	val infiniteLoop : () => Int  
		= 
		() => {
		  println( "infiniteLoop" )
		  infiniteLoop()
	}
		
	//val answer : Int = infiniteLoop() // will cause StackOverflowError
		
	val answer : ( () => Int ) => Int  
		=
		( f : () => Int ) => 42
		
	println( answer( infiniteLoop ) )
		
	
}