package sicp.recursive_and_tail_recursive.fakultaet

import annotation.tailrec

object TailrecursiveVersion extends Application {

  def facultaet( n :BigInt ) = {
    
    @tailrec def facIter( acc :BigInt, zaehler :BigInt ) :BigInt = {
      
      //println( acc + ", " + zaehler )
      
      if( zaehler > n ) acc else facIter( acc * zaehler, zaehler + 1 )
    }
    
    facIter( 1, 1 )
  }
  
  println( "facultaet 3 : " + facultaet( 3 ) )
  
  println( "facultaet 4 : " + facultaet( 4 ) )
  
  println( "facultaet 6 : " + facultaet( 5225 ) )
}