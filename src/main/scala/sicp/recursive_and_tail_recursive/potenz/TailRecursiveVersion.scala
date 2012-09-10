package sicp.recursive_and_tail_recursive.potenz

object TailRecursiveVersion extends Application{

  def pow( base :BigInt, exp :BigInt ) : BigInt = {
    
    def iter( acc :BigInt, zaehler :BigInt ) : BigInt =       
      if   ( zaehler == 0 ) acc
      else                  iter( base * acc, zaehler - 1 )
      
    iter( 1, exp )
  }
  
  val start = System.currentTimeMillis
  
  println( "pow(19,132) : " + pow( 1119, 21132 ) )
  
  val stop = System.currentTimeMillis
  
  println( "time : " + ( stop - start ) )
}