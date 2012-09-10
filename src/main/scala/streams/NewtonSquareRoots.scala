package streams

object NewtonSquareRoots {
	
	
	// a (genertal) stream for repeatedly applying a given argument (or better: the result of a given argument from the last application within the stream) 
	// to a given function
	// so the stream will deliver: a, f(a), f(f(a)), f(f(f(a))), ...
	def repeat[A]( f :A=>A, a :A ) :Stream[A] = Stream.cons( a, repeat( f, f(a) ) )
	
	// next n x = (x + n/x)/2
	// a (curried) function for iteratively calculating/approximating the square of a given number n
	def next( n :Double )( x :Double ) :Double = ( x + n/x ) / 2
	
	// a stream for iteratively better results for square calculation
	def approx( n :Double ) :Stream[Double] = repeat( next( n ), n )
	
	println( approx( 100 ).take( 10 ).force )
	
	// simply a function for returnung the absolute value of a given Double
	def abs( value :Double ) = if( value >= 0 ) value else value * -1
	
	
	// a function which 'traverses' the stream of square calculation, until the delta between two 
	// steps (two consequtive values, calculated by the stream) is under a given epsilon
	def within( eps :Double, stream :Stream[Double] ) : Double = stream match{
		
		case a #:: b #:: rest  if abs( a - b ) < eps => b
		case _ #:: b #:: rest => within( eps, b #:: rest )
	}
	
	// a function for returning the square of a given number n, where the result precision is 0.001
	def sqrt( a :Double ) :Double = within( 0.001, approx( a ) )
	
	println( "sqrt 49 : " + sqrt( 49 ) )
	
	
	def main( args :Array[String] ){
		
	}
}