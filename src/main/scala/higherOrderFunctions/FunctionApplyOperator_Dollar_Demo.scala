package higherOrderFunctions

object FunctionApplyOperator_Dollar_Demo {

	class RichFunction[-A,+B](fn: Function1[A,B]){ 
		def $(a:A):B = fn(a)
	}
  
	implicit def function2RichFunction[A,B](t: Function1[A, B]) = new RichFunction[A,B](t)
	
	
	val succ = (_:Int) + 1
	
	val double = (_:Int) * 2
	
	val add = (_:Int) + (_:Int)
	
	val d = double $ 3 + 4 + 9
	// instead of double( 3 + 4 + 9 )
	
	val next = succ $ 1
	
	//... but double $ succ & 3 
	// will not work like in Haskell (where $ is right asociative with lowest precedence)
	double $ (succ $ 3)
	
	
	
	println( "func application : " + next )
}