package typeclassopedia.functor

trait ApplicativeFunctor[F[_]] extends Functor[F]{

	def ffmap[A,B]( funct: F[A => B] )( fa: F[A] ) : F[B] 
	
	def pure[A]( a :A ) : F[A]
}