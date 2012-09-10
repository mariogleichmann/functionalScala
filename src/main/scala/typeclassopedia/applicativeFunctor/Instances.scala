package typeclassopedia.applicativeFunctor

import typeclassopedia.functor.instances.OptionFunctor
import typeclassopedia.functor.instances.ListFunctor
import typeclassopedia.monoid.Monoid
import typeclassopedia.monoid.instances._

object instances {



  abstract class Except[ Err:Monoid, +A ]
  case class OK[Err:Monoid,A]( a :A) extends Except[Err,A]
  case class Failed[Err : Monoid]( err :Err ) extends Except[Err,Nothing]
  
  class Validation[+A] extends Except[List[String],A]
  case class Valid[A]( a :A ) extends Validation[A]
  case class Invalid( errorCode :List[String] ) extends Validation[Nothing]
  
  implicit object ValidationAF extends ApplicativeFunctor[Validation]{
	  
	  def fmap[A,B]( func : A => B )( fa: Validation[A] ) : Validation[B] = fa match {
	 	  
	 	  case Valid( a ) 			=> Valid( func( a ) )
	 	  case invalid @ Invalid(_) => invalid
	  }
	   
	    def pure[A]( a :A ) : Validation[A] = Valid( a )
	  
	    
	  def ffmap[A,B]( funct: Validation[A => B] )( fa: Validation[A] ) : Validation[B] = {
    	
    	( funct,fa ) match {
    		
    		case ( Valid( f ), Valid( x ) )    			=> Valid( f( x ) )
    		case ( Valid( f ), invalid @ Invalid(x) )	=> invalid
    		case ( invalid @ Invalid(_), Valid(x) ) 	=> invalid
    		case ( Invalid( a ), Invalid( b ) )			=> Invalid( a ::: b )
    	}
    }
  }
  
  
  
//  type Partial[T[_,_], A] = {
//     type Apply[B] = T[A,B]
//  }
//  type MapS = Partial[Map, String]
//  type MapSI = MapS#Apply[Int]
//  
//  
//  type x = Validation[Int]#Apply[Int]
//  
//  def apply[A,T[_,_]] {
//	  
//	  type m = Partial[T,A]
//	   
//	   m
//  }
//  
//  implicit object ValidationAF extends ApplicativeFunctor[Validation]{
//	  
//	  
//	  
//	  def pure[A]( a :A ) : Validation[A] = OK[A]
//  }

  
//  implicit object ValidationAF extends ApplicativeFunctor[Except]{
//	  
//	  def fmap[A,B]( func : A => B )( fa: Validation[A] ) : Validation[B] = fa match {
//	 	  
//	 	  case OK( a ) => OK( func( a ) )
//	 	  case Failed( x ) => Failed( x )
//	  }
//  }
  
	
	
	
  implicit object OptionApplicativeFunctor extends ApplicativeFunctor[Option]{

	def fmap[A,B]( func : A => B )( fa: Option[A] ) : Option[B] = OptionFunctor.fmap(func)(fa) 
	
    def ffmap[A,B]( funct: Option[A => B] )( fa: Option[A] ) : Option[B] = {
    	
    	( funct,fa ) match {
    		
    		case ( None, _ )    => None
    		case ( _, None )    => None
    		case ( Some(f), Some(a) ) => Some( f(a) )
    	}
    }
    
    def pure[A]( a :A ) : Option[A] = Some( a )
    
  }
  

  // 'zip' version
  implicit object ListApplicativeFunctor extends ApplicativeFunctor[List]{
	  
	  def fmap[A,B]( func : A => B )( fa: List[A] ) : List[B] = ListFunctor.fmap(func)(fa)
	  
	  def ffmap[A,B]( funct: List[A => B] )( fa: List[A] ) : List[B] = {
	 	  
	 	  ( funct, fa ) match {
	 	 	  
	 	 	  case ( Nil, _ )	=> Nil
	 	 	  case ( _, Nil )	=> Nil
	 	 	  case ( f :: fs, a :: as ) => f(a) :: ffmap( fs )( as )
	 	  }
	  }
	   
	   def pure[A]( a :A ) : List[A] = a :: Nil
  }
  
  // 'mult' version ('cartesian product')
  implicit object ListApplicativeFunctorMult extends ApplicativeFunctor[List]{
	  
	  def fmap[A,B]( func : A => B )( fa: List[A] ) : List[B] = ListFunctor.fmap(func)(fa)
	  
	  def ffmap[A,B]( funct: List[A => B] )( fa: List[A] ) : List[B] = {
	 	  
	 	  ( funct, fa ) match {
	 	 	  
	 	 	  case ( Nil, _ )	=> Nil
	 	 	  case ( _, Nil )	=> Nil
	 	 	  case ( f :: fs, a ) =>  fmap(f)(a) ::: ffmap( fs )( a )
	 	  }
	  }
	   
	  //def flatten[A]( xxs : List[List[A]]) :List[A] = for( xs <- xxs; x <- xs ) yield x
	   
	   def pure[A]( a :A ) : List[A] = a :: Nil
  }
  
  
//  implicit def compose[FO[_]] :ApplicativeFunctor[FO] = new ApplicativeFunctor[FO]{
//	  
//	  def fmap[FI[_] : ApplicativeFunctor,A,B]( func : FI[A] => B )( foia: FO[FI[A]] ) : FO[B] = {
//	 	  
//	 	 (implicitly[ApplicativeFunctor[FO]]).fmap( func )( foia )
//	  }
//	  
//	   def ffmap[FI[_],A,B]( func: FO[FI[A] => B] )( foia: FO[FI[A]] ) : FO[B] = {
//	  	   
//	  	   (implicitly[ApplicativeFunctor[FO]]).ffmap( func )( foia )
//	   }
//	  
//	  
//	  def pure[FI[_] : ApplicativeFunctor,A]( a :A ) : FO[FI[A]] = {
//	 	  
//	 	  val inner = (implicitly[ApplicativeFunctor[FI]]).pure( a )
//	 	  
//	 	  (implicitly[ApplicativeFunctor[FO]]).pure( inner )
//	  }
//  }
  
  
  
  
  
  
  
  // ----------------------------------------------------------
  // some 'tools' for nice notation for using applicative functors + usage examples ...
  
  // lifiting a 'normal' (curried) function into the world of the applicative functor
  def pure[F[_] : ApplicativeFunctor,A]( a :A ) :F[A] = (implicitly[ApplicativeFunctor[F]]).pure( a )
  
  // a notational nicer way for lifting a normal function into the world of an applicative functor and applying it to a value which
  // also lives in the applicative functor world
  
  // an implicit conversion, taking a normal function and wrapping it into an instance of ApplicativeFuncUtils ...
  implicit def toApplicativeFunc[A,B]( func :A=>B ) = new ApplicativeFuncUtils[A,B]( func )
  
  // ... which provides a Function <%> ...
  class ApplicativeFuncUtils[A,B]( func :A=>B){
	  
	  // ...which takes a value of type A, which lives in an arbitrary applicative functor F[A]
	  // the instance / implementation of the applictive functor is forced to be given implicitly (as 'evidence' that F[_] is
	  // in fact an applicative functor)!
	  // -> now we can lift the normal function in the world of the applicative functor in which the value also lives
	  // (using the applicative functors 'pure' function) and then apply the lifted function to the value within the
	  // applicative world, using the applicative functors 'ffmap' function
	  def <%>[F[_] : ApplicativeFunctor]( fa :F[A] ) :F[B] = {
	 	  
	 	  val applicativeEvidence = implicitly[ApplicativeFunctor[F]]
	 	  val liftedFunc :F[A=>B] = applicativeEvidence.pure( func )
	 	  applicativeEvidence.ffmap( liftedFunc )( fa )	 	  
	  }
  }
  
  
  // the following implicit conversion is for functions, which already live in a certain applicative world
  // and now need to be applied to a value, which also lives in the same applicative world
  
  // therefore we'll take a lifted function F[A=>B], which already lives in the applicative world F[_],
  // while the instance / implementation of the applictive functor is forced to be given implicitly (as 'evidence' that F[_] is
  // in fact an applicative functor)! 
  // ... and wrapping it into an instance of ApplicativeApplyUtils ... (passing the lifted function and the applictive functor implementation/evidence)
  implicit def toApplicativeApply[F[_] : ApplicativeFunctor,A,B]( func :F[A=>B] ) = {
	  val applicativeEvidence = implicitly[ApplicativeFunctor[F]]
	  new ApplicativeApplyUtils[F,A,B]( func )( applicativeEvidence )
  }
  
  // ... which provides a function <*>
  class ApplicativeApplyUtils[F[_] : ApplicativeFunctor,A,B]( func :F[A=>B] ){
	  
	  // (extracting the implicit parameter - the applicative functor implementation, where the lifted function lives in)
	  val applicativeEvidence = implicitly[ApplicativeFunctor[F]]
	  
	   // ... which takes another value, which also have to live within the world of the same applicative functor (in which the function 
	   // already lives in)
	   // ... and then apply the value to the lifted function within the applicative world, using the applicative functors 'ffmap' function
	  def <*>( fa :F[A] ) :F[B] = {
	 	  
	 	  applicativeEvidence.ffmap( func )( fa )
	  }
  }
  

  
  // some conveniance methods for creating some Option-instances for Some and None which are of (super-)type Option 
  // (and not of sub-type Some or None)
  def some[A]( a :A ) :Option[A] = Some(a)
  def none :Option[Nothing] = None
  
  // and here are some examples ... 
  
  // first for Option as an applicative functor
  
  val add3 = (x:Int) => (y:Int) => (z:Int) => x + y + z	  
  
  val f1 :Option[Int=>Int=>Int] = add3 <%> some(3)
  
  f1 <*> some(4)
  
  println( "applicative add3 : " + ( add3 <%> some(3) <*> some(4) <*> some(7) ) )
  
  println( "applicative add3 with none : " + ( add3 <%> some(3) <*> none <*> some(7) ) )
  
  
  // applicative 'if'
  // the else-case will always be 'evaluated' even if the condition is true (and vice versa)
  // => this is different to the monadic 'if', where only the case will be evaluated which is selected by the condition
  def apiffy[F[_] :ApplicativeFunctor,A]( fb :F[Boolean] )( ft :F[A] )( ff :F[A] ) :F[A] = {
	  
	  val ifFunc = (cond :Boolean) => ( t :A) => ( f :A ) => if(cond) t else f
	  
	  ifFunc <%> fb <*> ft <*> ff	  
  }

  println( "apiffy : " + apiffy( some(true) )( some( "yep" ) )( some("nop") ) )
  println( "apiffy : " + apiffy( some(true) )( some( "yep" ) )( None ) ) // results into None from the false-case, even the condition selects the true-case!

  // ---
  
  def validateSuccess[A]( a :A ) :Validation[A] = Valid(a)
  def validateFailure[A]( error :String ) :Validation[A] = Invalid( error :: Nil )
  
  val valid = { (_:Int) + (_:Int) * (_:Int) }.curried <%> validateSuccess( 4 ) <*> validateFailure( "nop" ) <*> validateFailure( "fail" )
  println( "validation : " + valid )
  
  
  
  def main( args:Array[String] ) {
	  println("applicative functor instances");
	   
	   inner.printRes
	   
	   null
  }
   
}

  // ... and here some samples for list as an applicative functor
  // we needed to 'oursource' them into an own object, since we have 2 implicit implementations for list to be an applicative functor
  // sp that we can import only one of the implementations (otherwise the compiler will complain because of implicit ambiquity)
  object inner {
	  
	  import typeclassopedia.applicativeFunctor.instances.ListApplicativeFunctorMult
	  import typeclassopedia.applicativeFunctor.instances.toApplicativeFunc
	  import typeclassopedia.applicativeFunctor.instances.toApplicativeApply
	  import typeclassopedia.applicativeFunctor.instances.OptionApplicativeFunctor
	  import typeclassopedia.applicativeFunctor.instances.some
	  import typeclassopedia.applicativeFunctor.instances.none
	  
	   val add3 = (x:Int) => (y:Int) => (z:Int) => x + y + z	  
	  
	   val res = add3 <%> 1 :: 2 :: 3 :: Nil <*>  10 :: 20 :: 30 :: Nil <*> 100 :: 200 :: 300 :: Nil
	   
	   def printRes = println( "applicative on list : " + res )
	   
	   
	   
//	   val liofunc :List[Option[Int=>Int]] = List( some( (x:Int)=>x+1 ), some( (_:Int)*2 ), some( (x:Int)=>x*x ) )
//	   
//	   val liovals :List[Option[Int]] = List( some(1), some(2), some(3) )
//	   
//	   liofunc <%> liovals
//	   
//	   def lift[FO[_] : ApplicativeFunctor,FI[_] : ApplicativeFunctor,A,B]( fs :FO[FI[A=>B]] )( xs :FO[FI[A]] ) : FO[FI[B]] = {
//	  	   
//	  	   val fi = implicitly[ApplicativeFunctor[FI]]
//	  	   val fo = implicitly[ApplicativeFunctor[FO]]
//	  	    
//	  	   //def ffmap[A,B]( funct: F[A => B] )( fa: F[A] ) : F[B]
//	  	   fo.ffmap(  )
//	   }
  }