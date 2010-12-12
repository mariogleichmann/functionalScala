package typeclassopedia.functor

object Instances extends Application{

  implicit object ListFunctor extends Functor[List]{
    def fmap[A,B]( func : A => B )( fa: List[A] ) : List[B] = fa.map( func ) 
  }
  
  
  implicit object OptionFunctor extends Functor[Option]{
    def fmap[A,B]( func : A => B )( fa: Option[A] ) : Option[B] = fa.map( func )
  }
  
  implicit object optionApplicativeFunctor extends ApplicativeFunctor[Option]{
    def fmap[A,B]( func : A => B )( fa: Option[A] ) : Option[B] = fa.map( func )

    def ffmap[A,B]( funct: Option[A => B] )( fa: Option[A] ) : Option[B] = {
    	
    	( funct,fa ) match {
    		
    		case ( None, _ )    => None
    		case ( _, None )    => None
    		case ( Some(f), Some(a) ) => Some( f(a) )
    	}
    }
    
    def pure[A]( a :A ) = Some( a )
  }
  
  val add3 = (x:Int) => (y:Int) => (z:Int) => x + y + z
  
  val x = optionApplicativeFunctor.ffmap( Some(add3) )(Some(3))
  val y = optionApplicativeFunctor.ffmap( x )(None)
  val z = optionApplicativeFunctor.ffmap( y )(Some(5))
  
  println(z)

  //val zz = optionApplicativeFunctor.ffmap( z )(Some(5))

  
  implicit def toRichAppFunctor[F[_],A,B]( funcInFunct : F[A=>B] )( implicit functor :ApplicativeFunctor[F] ) = {
    new RichAppFunctor[F,A,B]( funcInFunct, functor )
  }
  
  class RichAppFunctor[F[_],A,B]( funcInFunct : F[A=>B], functor :ApplicativeFunctor[F] ){
	  def compose( fa: F[A] ) : F[B] = functor.ffmap( funcInFunct )( fa )
  }
  
  def pure[F[_],A]( a :A )( implicit functor :ApplicativeFunctor[F] ) = functor.pure( a )
  
 
 
  
  //val cc = (Some(add3):Option[Int=>Int=>Int=>Int]) compose Some(3) compose Some(4) compose Some( 5 )
  val cc = pure( add3 ) compose Some(3) compose Some(4) compose Some( 5 )
 
  println( "cc: " + cc )
  
  
  val pureAdd3 = pure( add3 )
  
  val dd = pureAdd3 compose Some( 3 ) compose None compose Some( 5 )
  
  println( "dd: " + dd )
  
  
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
}
