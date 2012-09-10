package sicp.recursive_and_tail_recursive.potenz

object RecursiveVersion extends Application{

  def pow( base :BigInt, exp :BigInt ) : BigInt = if( exp <= 0 ) base else base * pow( base, exp - 1 )
  
  println( pow( 1119, 21132 ) )
}