package typeclassopedia.functor

trait Functor [F[_]]{
    
  def fmap[A,B]( func : A => B )( fa: F[A] ) : F[B]  
}