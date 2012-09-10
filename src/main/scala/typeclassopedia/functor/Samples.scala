package typeclassopedia.functor

import typeclassopedia.functor.instances._

object Samples {

  // --- (1) --- lifing
	
  val liftedSucc = lift(  (x:Int) => x+1  ).to[Option]		// 1. Möglichkeit
  
  val liftedDouble = (  (x:Int) => x+x  ).liftTo[Option]	// 2. Möglichkeit
  
  val liftedTriple = liftTo[Option] << ( (x:Int) => x*3 )	// 3. Möglichkeit
  
  val liftedLength = liftTo[List] << ( (x:String) => x.length )
  
  
  println( "lifted Succ : " + liftedSucc( Some( 1 ) )  )
  
  println( "lifted double none : " + liftedDouble( None )  )
  
  println( "lifted double : " + liftedDouble( Some( 4 ) )  )
  
  println( "lifted triple : " + liftedTriple( Some( 4 ) )  )
  
  println( "lifted length : " + liftedLength( List( "Hello", "world", "!!!" ) )  )
  
  
  // --- (2) --- syntax sugar
  
  def some[A]( a :A ) : Option[A] = Some(a)
  
  some( "hello" ) fmap ( s => s.length ) fmap( (x:Int) => x + 1 ) fmap ( _ * 2 )
  
  
	
  def main( args :Array[String] ){
		println( "samples" )
	}
}