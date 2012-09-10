package typeclassopedia.traversable

import typeclassopedia.applicativeFunctor.ApplicativeFunctor

trait Traversable[T[_]] {

	//traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
	def traverse[AF[_],A,B] ( f : A => AF[B] )( traversable :T[A] )( implicit apFunctor :ApplicativeFunctor[AF] ) : AF[T[B]]
	
	// sequence :: Applicative f => t (f a) -> f (t a)  // also called 'dist' for distributing 
	def sequence[AF[_],A]( traversable :T[AF[A]] )( implicit functor :ApplicativeFunctor[AF] ) : AF[T[A]] 
	
}