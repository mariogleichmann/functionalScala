package origami

object OrigamiInvest extends Application{

  // a simple list as datatype impl
	
  trait SimpleList[+A]
  case object Nil extends SimpleList[Nothing]
  case class Cons[A] (x :A, xs : SimpleList[A]) extends SimpleList[A]
  
  // foldL is for 'foldList', not foldLeft!
  
  def foldL[A,B]( f : A => B => B )( unit :B )( xs :SimpleList[A] ) : B = xs match {	  

    case Nil 			=> unit
    
    case Cons( y, ys )  => f( y )( foldL( f )( unit )( ys ) ) 
  }
  
  // insertion-sort as fold -------------------------------------------
	
  case class I( i :Int ) extends Ordered[I]{
	  override def compare( other :I ) = i - other.i
  }
  
  
  // function, which inserts the given element at the right position (due to ordering)
  def insert[A <: Ordered[A]]( x :A )( xs :SimpleList[A] ) : SimpleList[A] =
	xs match {
	  case Nil 		     => Cons( x, Nil )
	  case Cons( y, ys ) => if( x < y ) Cons( x, Cons( y, ys ) )
	                         else Cons( y, insert( x )( ys ) )
  }
  
  // here's the insertion-sort, delegating to foldL
  def isort[A <: Ordered[A]]( xs :SimpleList[A] ) : SimpleList[A] = foldL( insert[A] )( Nil )( xs )
  
  val list1 = Cons( I(8) ,Cons( I(5), Cons( I(9), Cons( I(3), Nil ) ) ) )  
  println( isort( list1 ) )
  
  // list-append as fold ------------------------------------------------
  
  def prepend[A]( x :A )( xs :SimpleList[A] ) : SimpleList[A] = Cons( x, xs )
  
  def append[A]( xs :SimpleList[A] )( ys :SimpleList[A] ) : SimpleList[A] = foldL( prepend[A] )( ys )( xs )
	  
  val listA = Cons( I(8) ,Cons( I(5), Cons( I(9), Cons( I(3), Nil ) ) ) )  
  val listB = Cons( I(18) ,Cons( I(15), Cons( I(19), Cons( I(13), Nil ) ) ) )
  
  println( append( listA)( listB ) )
  
  // list-concat (flatten) as fold -----------------------------------------
  
  def concat[A]( xss :SimpleList[SimpleList[A]] ) : SimpleList[A] = foldL( append[A] )( Nil )( xss )
  
  val listC = Cons( I(28) ,Cons( I(25), Cons( I(29), Cons( I(23), Nil ) ) ) )
  
  val listlist = Cons( listA, Cons( listB, Cons( listC, Nil ) ) )
  
  println( concat( listlist ) )
  
}