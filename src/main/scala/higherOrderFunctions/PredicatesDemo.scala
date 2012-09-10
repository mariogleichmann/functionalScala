package higherOrderFunctions

object PredicatesDemo extends Application{

  type Predicate[A] = A => Boolean

  def lift[A]( f: (Boolean, Boolean) => Boolean ) : (Predicate[A], Predicate[A]) => Predicate[A] =
	( pa, pb ) => ( a :A ) => f( pa( a ), pb( a ) )
 
  def and[A] = lift[A] ( _ && _ )

  def or[A] = lift[A] ( _ || _ )


  def orDirect[A]( pa:Predicate[A], pb:Predicate[A] ) : Predicate[A]  =    a => pa(a) || pb(a) 

  def not[A]( p :Predicate[A] ) : Predicate[A] =  a => !p(a)


  def isDivisableBy( k :Int ) : Predicate[Int] =  x => x % k == 0
  
  val isEven = isDivisableBy( 2 )
  
  val isOdd = not( isEven )
  
  val taut = or( isOdd, isEven )
  
  implicit def toRichPred[A]( pred :Predicate[A] ) = new Object{
	  def or( p :Predicate[A] ) = PredicatesDemo.or( pred, p )
	  def and( p :Predicate[A] ) = PredicatesDemo.and( pred, p )
  }
  
//  val taut2 = isOdd or isEven
//  
//  val taut3 = isOdd or not( isOdd )
//  
//  println( taut2( 3 ) )
//  println( taut2( 2 ) )
}