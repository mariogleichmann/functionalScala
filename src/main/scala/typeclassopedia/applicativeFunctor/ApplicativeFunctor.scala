package typeclassopedia.applicativeFunctor

import typeclassopedia.functor.Functor

trait ApplicativeFunctor[F[_]] extends Functor[F]{

	def ffmap[A,B]( funct: F[A => B] )( fa: F[A] ) : F[B] 
	
	def pure[A]( a :A ) : F[A]
}