package sicp.recursive_and_tail_recursive.fibonacci

object RecursiveVersion extends Application{


  
  def fib( n :BigInt ) :BigInt = {
    
    if 		( n == 0 )  0
    else if	( n == 1 )  1
    else 				fib( n - 1) + fib( n - 2 )
  }
  
  
  println( "fib 43 : " + fib( 143 ) )
}