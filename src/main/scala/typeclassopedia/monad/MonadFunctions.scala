package typeclassopedia.monad

import typeclassopedia.monad.Monad._

object MonadFunctions {

	  // monadic 'if'
  // the else-case will not be 'evaluated' if the condition is true (and vice versa)
  // => this is different to the applicative 'if', where both cases will always be evaluated 
  // (and may result to None, even the 'None-making-case' isn't selected by the condition!!!)
  def miffy[M[_] : Monad, A]( mb :M[Boolean] )( mt :M[A] )( mf :M[A] ) : M[A] = {
	  
	  mb >>= ( cond => if( cond ) mt else mf )
  }
  
  //see book 'Real World Haskell' page 214
  def mapM[M[_] : Monad, A, B] ( f : A => M[B] )( as : List[A] )  : M[List[B]] = {
	  
	  val monadEvidence = implicitly[Monad[M]]
	  
	  as match {
	 	  
	 	  case Nil        => monadEvidence.Return( Nil )
	 	  
	 	  case a :: atail => {
	 	 	  					val mb : M[B] = f( a )
	 	 	         
	 	 	  					val mbs : M[List[B]] = mapM( f )( atail )
	 	 	  					
	 	 	  					for( bs  <- mbs;
	 	 	  						 b   <- mb ;
	 	 	  						 ret <- monadEvidence.Return( b :: bs ) ) yield ret

//                              or alternatively :
//	 	 	  					mbs >>= ( bs => 
//	 	 	  					mb  >>= ( b  => 
//	 	 	  					
//	 	 	  					monadEvidence.Return( b :: bs ) ) )
	 	  					 }
	  }	  
  }
  
  // same as mapM, only arguments are switched !!!
  def forM[M[_] : Monad, A, B] ( as : List[A] )( f : A => M[B] )  : M[List[B]] = {
	  
	  val monadEvidence = implicitly[Monad[M]]
	  
	  as match {
	 	  
	 	  case Nil        => monadEvidence.Return( Nil )
	 	  
	 	  case a :: atail => for( bs  <- forM( atail )( f );
	 	 	  					  b   <- f( a ) ;
	 	 	  					  ret <- monadEvidence.Return( b :: bs ) ) yield ret

	 	  					
	  }	  
  }
  
  //see book 'Real World Haskell' page 219
  def filterM[M[_] : Monad, A]( predM : A => M[Boolean] )( as : List[A] ) : M[List[A]] = {
	  
	  val monadEvidence = implicitly[Monad[M]]
	   
	  as match {
	 	  
	 	  case Nil			=> monadEvidence.Return( Nil )
	 	  
	 	  case a :: atail   => { 
	 	 	  				     val mas : M[List[A]] = filterM( predM )( atail )
	 	 	  				     
	 	 	  				     val filterResult = predM( a )
	 	 	  				     
	 	 	  				     mas          >>= ( as   =>  
	 	 	  				     filterResult >>= ( isOk => 
	 	 	  				     					
	 	 	  				       if( isOk ) monadEvidence.Return( a :: as ) else monadEvidence.Return( as ) ) )	 
	 	  					   }
	  }
  }
  
  // lifting a 'normal' function into the context of a Monad (applying the function to the 'value' inside of the Monad)
  // vgl. Applicative Functor !!!  
  def liftM[M[_] : Monad, A, B]( f : A => B )( ma :M[A] ) : M[B] = {
	  
	  val monadEvidence = implicitly[Monad[M]]
	  
	  for( a   <- ma;
	       ret <-  monadEvidence.Return( f( a ) ) ) yield ret
  }
  																	
  
}