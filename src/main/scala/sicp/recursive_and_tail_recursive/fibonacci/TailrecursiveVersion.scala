package sicp.recursive_and_tail_recursive.fibonacci

object TailrecursiveVersion extends Application{

  def fib( n :Int ) :BigInt = {
    
    def iter( a :BigInt, b :BigInt, zaehler :Int ) : BigInt = {
      
      println( a + ", " + b + ", " + zaehler ); 
      
      if( zaehler == 0 ) a else iter( b, a + b, zaehler - 1 )
    }
    
    iter( 0, 1, n )
  }
  
  println( "fib 63 : " + fib( 143 ) )
  
}