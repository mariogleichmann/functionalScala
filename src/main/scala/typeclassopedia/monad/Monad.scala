package typeclassopedia.monad

trait Monad [M[_]] {

	def Return[A]( a :A ) : M[A]
	
	def bind[A,B]( ma : M[A] )( f : A => M[B] ) : M[B]
	
	def chain[A,B]( ma : M[A] )( f : => M[B] ) : M[B] = bind( ma )( _ => f )
	
	def fail[A]( msg : String ) : M[A] = error( msg )
}


object Monad{
  
  // some utils
  
  	class MonadUtils[M[_] : Monad,A]( ma :M[A] ){
		
		val monadEvidence = implicitly[Monad[M]]
		
		def >>= [B]( f: A => M[B] ) : M[B] = monadEvidence.bind( ma )( f )
		
		def >> [B] ( f : => M[B] ) : M[B] = monadEvidence.chain( ma )( f )
		
		//def Return[B]( b :B ) = monadEvidence.Return( b )
		
		
		// the following methods are needed to enable Monads to participate in for-comprehensions
		// (for using guards in for comprehensions, we also would need to define a filter method
		def flatMap [B]( f: A => M[B] ) : M[B] = monadEvidence.bind( ma )( f )
		
		def map[B]( f: A => B ) : M[B] = flatMap { x => monadEvidence.Return( f(x) ) }
		
		def foreach[B]( f: A=>B) :Unit = { map( f) } 
	}

  // an implicit conversion for any type M[_], for which's a Monad typeclass implementation available
  // (making that type M[_] an instance of typeclass Monad)
  // wraps that instance into MonadUtils, which provides some convenient notational forms of calling the monad functions
  implicit def toMonadUtils[M[_] : Monad,A]( ma :M[A] ) = {
	  val monadEvidence = implicitly[Monad[M]]
	  new MonadUtils[M,A]( ma )( monadEvidence )
  }
  
  def Return[M[_]:Monad,A]( a :A ) :M[A] = implicitly[Monad[M]].Return( a )
  

  // ----------
  
  // instances:  
  
	
	implicit object OptionMonad extends Monad[Option]{
		
		def Return[A]( a :A ) = Some( a )
		
		def bind[A,B]( ma : Option[A] )( f : A => Option[B] ) : Option[B] = ma match {
			
			case None => None			
			case Some( a ) => f( a )
		}		
	}
	
	
	implicit object ListMonad extends Monad[List]{
		
		def Return[A]( a :A ) = a :: Nil
		
		def bind[A,B]( ma : List[A] )( f : A => List[B] ) : List[B] = ma match {
			
			case Nil => Nil
			
			case x :: xs => f( x ) ::: bind( xs )( f )
		}	
	}
	
	
	
	import typeclassopedia.monad.Parser._
	
	implicit object ParserMonad extends Monad[Parser]{
	  
	  	def Return[A]( a :A ) =   inp => List( (a,inp) )
		
		def bind[A,B]( pa : Parser[A] )( f : A => Parser[B] ) : Parser[B] = 
		  
		  inp =>  pa( inp ) match {
		    case Nil 			=> Nil
		    case (v,out) :: Nil => f( v )( out )
		  }
	}
	
	
	
	
	import typeclassopedia.monad.Either._
	
	type ProjectTo[A, F[_,_]] = {
	  type Apply[B] = F[A,B]
	}
	
	
	trait PartialApply1Of2[T[_,_], A] {
		
	    type Apply[B] = T[A, B]

		type Flip[B] = T[B, A]
	}
	
	implicit def toEitherMonad[L,R]( e :Either[L,R] ) = {
 
		type BoundEither[RI] = ProjectTo[L,Either]#Apply[RI]
 
		class BoundEitherMonad extends Monad[BoundEither]{
		  
			def Return[R]( r :R )= Right[L,R]( r )
 
			def bind[R,S]( e :BoundEither[R] )( f : R => BoundEither[S] ) =
				e match {
					case Left( l ) => Left( l )
					case Right( r ) => f( r )
				}
			}
			implicit val m = new BoundEitherMonad

			m
		}

	
  // The Left type (the 'error' type) is bound to E by the surrounding function eitherMonad
  implicit def eitherMonad[E]() =  new Monad[( { type EitherE[r] = Either[E,r] } )#EitherE] {
	  
	  def Return[R]( r :R )= Right[E,R]( r )
	  
	  def bind[A,B]( ma : Either[E,A] )( f : A => Either[E,B] ) : Either[E,B] =  ma match {
	 	  
	 	  case Left( e )  =>  Left( e )
	 	  
	 	  case Right( a ) =>  f( a )
	  }
  }

		
		
//		def either[L,R]( e: Either[L,R] ) : ProjectTo[L,Either]#Apply[R] = e
		
//		implicit def tooooEitherMonad[Either[_,_],L,R]( e :Either[L,R] ) = {
//		
//		  object BoundEitherMonad extends Monad[ProjectTo[L,Either]#Apply[R]]{
//		  
//			def Return( r :R )= Right[L,R]( r )
// 
//			def bind[S]( e :ProjectTo[L,Either]#Apply[R] )( f : R => ProjectTo[L,Either]#Apply[S] ) =
//				e match {
//					case Left( l ) => Left( l )
//					case Right( r ) => f( r )
//				}
//			}
//		  BoundEitherMonad
//		}
		
	//var e1 = either[String,Int]( Right(1) )
	var e1 = Right(1)
	
//	var e2 = e1 >>= ( i => Right( i+1 ) )
	
	
//((if condition then x else y), z) == if condition then (x,z) else (y,z)
//     lazy-able							NON-lazy-able

	
	
  import typeclassopedia.monad.State._
	
  // This is the implementation for making type State a member of the monad typeclass :
  
  // we need 'partial type application', since State is a type constructor of kind (*,*) -> *, 
  // but the Monad typeclass asks for a type constructor of kind * -> *
	
  // Type StateS is a Type constructor of kind * -> * !!! 
  // Within the type definition of StateS (which itself is defined within an anonymous structural type, delimeted by {...} ),
  // the type S is already fixed (because it is introduced outside of that anonymous structural type and therefore is fixed at runtime,
  // when function stateMonad is called for a then given type S - the State type is bound to S by the surrounding function stateMonad)
  // The only 'open' Type then is 'a' (for any given type 'a') for our type StateS (which makes StateS to be a type construcor of kind * -> * )
  // Since we're only interested in StateS, we do a type projection upon the anonymous structural type to receive the type definition
  // of StateS (which is defined inside the anonymous structural type) to pull it out and reference to it
  
  def stateMonad[S]() =  new Monad[( { type StateS[a] = State[a,S] } )#StateS] {
	  
	def Return[A]( a :A ) : State[A,S] = State( (s :S ) => ( a, s ) )
	
	def bind[A,B]( ma : State[A,S] )( f : A => State[B,S] ) : State[B,S] = 
		
		State ( ( s :S ) => {  
			
		  val ( a, newState ) = ma.runState( s ) 
			
		  f( a ).runState( newState )	
		  
		} )				
  }


  implicit def toStateMonad[T, S](state: State[T,S]) = 
	new MonadUtils[( { type StateS[a] = State[a,S] } )#StateS,T]( state )( stateMonad[S] )

	 
	

 

  
}
