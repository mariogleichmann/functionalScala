package sicp.recursive_and_tail_recursive.fakultaet

object RecursiveVersion extends Application{

  def facultaet( n :BigInt ) : BigInt = if( n <= 0 ) 1 else ( n * facultaet( n - 1 ) )
  
  println( " f : " + facultaet( 3225 ) )
}