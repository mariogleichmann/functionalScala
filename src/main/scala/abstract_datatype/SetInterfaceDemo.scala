package abstract_datatype

object SetInterfaceDemo extends Application{

  trait SimpleList[+A]
  case object Nil extends SimpleList[Nothing]
  case class Cons[A] (x :A, xs : SimpleList[A]) extends SimpleList[A]
  
  def len[A]( l : SimpleList[A] ) : Int = l match {
	case Nil => 0
	case Cons (x, xs)=> 1 + len(xs)
  }
  
  def ins[A <: Ordered[A]]( x :A, xs :SimpleList[A] ) : SimpleList[A] =
	xs match {
	  case Nil 		     => Cons( x, Nil )
	  case Cons( y, ys ) => if( x < y ) Cons( x, Cons( y, ys ) )
	                         else Cons( y, ins( x, ys ) )
  }
		
	
	
  trait SetInterface{
    
	type S[_]
    type A

    def empty : S[A]
    def insert( x :A, s :S[A] ) : S[A]
    def extract( s :S[A] ) : Option[ ( A,S[A] ) ]
  }
  
  
  trait OrderedSet extends SetInterface{
    
	type S[X] = SimpleList[X]
    type A <: Ordered[A]
	
    def empty = Nil
    
    def insert( x :A, xs :S[A] ) = ins( x, xs )
    
    def extract( xs :S[A] ) = xs match {
      case Nil   	    => None
      case Cons (y, ys) => Some ( y, ys )
	}
  }
  
//  class OrdSet[X <: Ordered[X]] extends OrderedSet{
//	  type A = X
//  }
  
  
  case class Elem( nr :Int ) extends Ordered[Elem]{
	  override def compare( other :Elem ) = nr - other.nr
  }

  
  val eSet :OrderedSet{ type A = Elem } = new OrderedSet{ type A = Elem }
//  val eSet2 = new OrdSet[Elem]
  
  //eSet.insert( Elem(1), eSet )
  

}