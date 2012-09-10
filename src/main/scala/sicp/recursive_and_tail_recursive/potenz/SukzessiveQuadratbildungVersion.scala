package sicp.recursive_and_tail_recursive.potenz

object SukzessiveQuadratbildungVersion extends Application  {
// z.B. pow( b, 8 ) = pow( b, 4 ) * pow( b, 4 )
  
  def even( n :BigInt ) = n % 2 == 0
  
  def quadrat( n :BigInt ) = n * n
  
  def pow( base :BigInt, exp :BigInt ) :BigInt = {
    
    if      ( exp == 0 )     1
    else if ( even( exp ) )	 quadrat( pow( base, exp / 2 ) )
    else 					 base * pow( base, exp - 1 )
  }
  
  
  val start = System.currentTimeMillis
  
  println( "pow(19,132) : " + pow( 1119, 21132 ) )
  
  val stop = System.currentTimeMillis
  
  println( "time : " + ( stop - start ) )
  
  
 
  
  
  
  
  
  
  
  
}