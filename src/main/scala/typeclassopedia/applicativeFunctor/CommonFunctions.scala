package typeclassopedia.applicativeFunctor

object CommonFunctions {

  implicit def toRichAppFunctor[F[_],A,B]( funcInFunct : F[A=>B] )( implicit functor :ApplicativeFunctor[F] ) = {
    new RichAppFunctor[F,A,B]( funcInFunct, functor )
  }
  
  class RichAppFunctor[F[_],A,B]( funcInFunct : F[A=>B], functor :ApplicativeFunctor[F] ){
	  def compose( fa: F[A] ) : F[B] = functor.ffmap( funcInFunct )( fa )
  }
  
  def pure[F[_],A]( a :A )( implicit functor :ApplicativeFunctor[F] ) : F[A] = functor.pure( a )
  
  
  def prepend[A]( x :A )( xs :List[A] ) = x :: xs
  
  def sequence[F[_],A]( seq :List[F[A]] )( implicit functor :ApplicativeFunctor[F] ) : F[List[A]] = seq match {
	  
	  case Nil 			=> functor.pure( Nil )
	  
	   //(x:xs) = (:) <$> x <*> sequenceA xs
	   
	   case ( x :: xs )  => {	// just 'lift' prepend into the effectful world ... 
	 	  						val prependAsFunc = pure( prepend[A] _ )	  	   						
	 	  						
	 	  						// ... and then apply it to the elements inside the applicative 'container' ...
	 	  						functor.ffmap( functor.ffmap( prependAsFunc )( x ) )( sequence( xs ) )
	  					  }
  }
  
// flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
//flakyMap f ss = dist (fmap f ss)
  
  def flakyMap[F[_],A,B]( f: A => F[B] )( lst :List[A] )( implicit functor :ApplicativeFunctor[F] ) : F[List[B]] = {
	  
	  sequence( lst.map( f ) ) 
  }
 
// traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
// traverse f [ ] = J [ ] K
// traverse f (x : xs) = J (:) (f x ) (traverse f xs) K
  
  def traverse[F[_],A,B] ( f : A => F[B] )( lst :List[A] )( implicit functor :ApplicativeFunctor[F] ) : F[List[B]] = lst match {
	  
  	case Nil 		 => functor.pure( Nil ) 
  	
  	case ( x :: xs ) =>	{
  							val prependAsFunc = pure( prepend[B] _ )
  							
  							val fst :F[B] = f( x )
  							
  							functor.ffmap( functor.ffmap( prependAsFunc )( fst ) )( traverse( f )( xs ) )
  						}
  }
  
   
  
  import typeclassopedia.applicativeFunctor.instances.OptionApplicativeFunctor
  
    // some conveniance methods for creating some Option-instances for Some and None which are of (super-)type Option 
  // (and not of sub-type Some or None)
  def some[A]( a :A ) :Option[A] = Some(a)
  def none :Option[Nothing] = None
  
  
  val optSeqs = sequence( some(3) :: some(5)  :: None :: (Some(9) :Option[Int]) :: Nil )
  println( "optseqs ...:" + optSeqs )
  
  
  
  def main( args :Array[String]){
	  
  }
}