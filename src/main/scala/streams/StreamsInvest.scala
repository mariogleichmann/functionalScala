package streams

object StreamsInvest extends Application {

	val ones : Stream[Int] = Stream continually 1	
	val x : Stream[Int] = ones.take( 10 )	
	//println( x.foreach( println _ ) )
	
	
  def diceThrow :Int = util.Random.nextInt(6) + util.Random.nextInt(6) + 2
  def diceStream :Stream[Int] = Stream continually diceThrow
  
  val results :Map[Int,Stream[Int]] = diceStream take 200 groupBy identity
  
//  println( "foreach ..." )
//  results foreach( println _ )
  
  2 to 12 foreach { result =>
    println( result + ":" + "X" * results( result ).size)
  }
	
	
	// ----------------
	
	def range(start: Int, end: Int): Stream[Int] = {
	  println( "range " + start + " - " + end )
      if (start >= end) Stream.empty
      else Stream.cons(start, range(start + 1, end) )
	}
      
    def list(start: Int, end: Int): List[Int] = {
      println( "list " + start + " - " + end )
      if (start >= end) Nil
      else ::(start, list( start + 1, end ) )  
    }
	
    println( "list build ..." )
    val li = list( 2, 17 )
    println( "list print ..." )
    println( li )
    
    println( "range build ..." )
    val ra = range( 2, 17 )
    println( "range print ..." )
    println( ra )
    println( "ra 4" )
    println( ra( 4 ) )
    
    // --------------
    
    lazy val fib :Stream[Int] = Stream.cons( 0, Stream.cons( 1, fib.zip( fib.tail ).map( p => p._1 + p._2) ) )
    // fib is for      0 1 1 2 3 5 8 13 ...
    // fib.tail is for 1 1 2 3 5 8 13
    // fib.zip( fib.tail ) results in a list of pairs which will pair the first elements of both Streams, next pair the second elements of
    // both streams, next the third elements ... etc
    // ... so we end up with a Stream of pairs: (0,1) (1,1) (1,2) (2,3) ... etc
    // then we map each pair within that new stream to the sum of its elements
    // ... so we finally end up with 1 2 3 5 ... etc (for the rest of the stream, after the first two elements 0 and 1)
    
    println( ( fib take 10 ) force )
    
    // ---------------
    
   lazy val naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))
   
   // ---------------
    
   def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

   def sieve( s: Stream[Int] ): Stream[Int] = Stream.cons( s.head, sieve( s.tail filter { _ % s.head != 0 } ) )

   def primes = sieve( from( 2 ) )

   primes take 10 print

    
    
    

     
}