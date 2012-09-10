package typeclassopedia.functor

object instances {

  implicit object ListFunctor extends Functor[List]{
    def fmap[A,B]( func : A => B )( fa: List[A] ) : List[B] = fa.map( func ) 
  }
  
  
  implicit object OptionFunctor extends Functor[Option]{
    def fmap[A,B]( func : A => B )( fa: Option[A] ) : Option[B] = fa.map( func )
  }
  

 


  
  
//  val add3 = (x:Int) => (y:Int) => (z:Int) => x + y + z
//  
//  val x = optionApplicativeFunctor.ffmap( Some(add3) )(Some(3))
//  val y = optionApplicativeFunctor.ffmap( x )(None)
//  val z = optionApplicativeFunctor.ffmap( y )(Some(5))
//  
//  println(z)
//
//  

  

//  val add3 = (x:Int) => (y:Int) => (z:Int) => x + y + z	  
//  val cc = (Some(add3):Option[Int=>Int=>Int=>Int]) compose Some(3) compose Some(4) compose Some( 5 )
//  val cc = pure( add3 ) compose Some(3) compose Some(4) compose Some( 5 )
// 
//  println( "cc: " + cc )
//  
//  
//  val pureAdd3 = pure( add3 )
//  
//  val dd = pureAdd3 compose Some( 3 ) compose None compose Some( 5 )
//  
//  println( "dd: " + dd )
//  
  
  object EitherExc{
    
    type EitherExcOr[T] = Either[Exception,T]
    
    implicit object EitherExcOrFunctor extends Functor[EitherExcOr]{
      def fmap[A,B]( func : A => B )( eitherExc: EitherExcOr[A] ) : EitherExcOr[B] = {
     
        eitherExc match {
          case Left( x :Exception ) => Left(x)         
          case Right( y :A ) 		=> Right( func(y) )
        }
      } 
    }   
  }
  
  // ----- Tools -------------------------------------------
  
  
  // --- (1) --- 'lifting' a pure Function A=>B to F[A] => F[B]
  
  // 1.Möglichkeit
  def lift[A,B]( func :A=>B ) = new FunctionLifter( func )
  
  class FunctionLifter[A,B]( func :A=>B ){
	  
	  def to[ F[_] :Functor ] : F[A] => F[B] = {
	 	  
	 	  val functorEvidence :Functor[F] = implicitly[Functor[F]]
	 	   
	 	  functorEvidence.fmap( func )	 	   
	  }
	   
	  def liftTo[ F[_] :Functor ] : F[A] => F[B] = to[F]
  }
  
  // 2.Möglichkeit
  implicit def toLiftingFunc[A,B]( func :A=>B ) = new FunctionLifter( func )
  
  
  
  // 3.Möglichkeit
  def liftTo[ F[_] :Functor ] = new Lifter[F]
  
  class Lifter[F[_] :Functor]{
	  
	  val functorEvidence :Functor[F] = implicitly[Functor[F]]
	   
	  def <<[A,B]( func :A=>B ): F[A] => F[B] = {
	 	  
	 	  functorEvidence.fmap( func )	
	  }
  }
  
  
  // --- (2) --- a nice notation for chaining mapping
  
  	class FunctorWrapper[F[_] : Functor,A]( fa :F[A] ){
		
		val functorEvidence = implicitly[Functor[F]]
		
		def fmap[B]( func : A => B ) : F[B] = functorEvidence.fmap( func )( fa )		
	}

  // an implicit conversion for any type F[_], for which's a Functor typeclass implementation available
  // (making that type F[_] an instance of typeclass Functor)
  // wraps that instance into FunctorWrapper, which provides some convenient notational forms of calling the Functor function
  implicit def toFunctorWrapper[F[_] : Functor,A]( fa :F[A] ) = {
	  val functorEvidence = implicitly[Functor[F]]
	  new FunctorWrapper[F,A]( fa )( functorEvidence )
  }
  
}


	
	
  
 
