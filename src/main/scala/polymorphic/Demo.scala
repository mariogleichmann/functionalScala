package polymorphic

object Demo {
	
	case class Person( name:String, alter:Int )
	
	
	class Id[T] extends Function1[T,T]{
		def apply( x :T ) : T = x
	}

	def id[T]( t :T ) :T = t
	
	def createIdFunc[T] = ( t :T ) => t
	
	def transform[A]( a :A, trans : A => A  ) = trans( a )
	
	
	
	
	class First[T] extends Function1[Tuple2[T,_],T]{
		def apply( pair : Tuple2[T,_] ) :T = pair._1
	}
	
	val firstString = new First[String]()
	
	val cc : String = firstString( ( "x",1) )
	
	
	val empty = ( xs :List[_] ) => xs == Nil
	
	
	
	println( "empty List() " + empty( List() ) )
	println( "empty Nil " + empty( Nil ) )
	println( "empty List(1,2) " + empty( List(1,2) ) )	
	
	// you can define a function using existential types (for some type)
	val firstUntyped : ( (Any,_) ) => _ = ( pair : Tuple2[_,_] ) => pair._1	
	// you could've also declared this function type : ( (_,_) ) => Any
	// or this type : ( (Any,Any) ) => Any
	
	// ... but then you'll receive no specific type (so the return type is Any)
	val uf : Any  = firstUntyped ("x",2)
	
	// this is a method ... (which may be used as a function by leveraging eta expansion)
	def first[A,B]( pair :(A,B) ) = pair._1
	
	println( "first: " + first(1,2) )
	
	val s1st :String = first("x",1)
	val i1st :Int = first(1,"x")
	val p1st :Person = first( Person("hans",31), 1 )
	val l1st :List[String] = first( List("v","c","s"), 1 )
	
	// and this a method, producing a function
	def firstFunc[A] = ( pair :(A,_) ) => pair._1
	
	println( "firstFunc: " + firstFunc(1,2) )
	
	val s1stFunc :String = firstFunc("x",1)
	val i1stFunc :Int = firstFunc(1,"x")
	val p1stFunc :Person = firstFunc( Person("hans",31), 1 )
	val l1stFunc :List[String] = firstFunc( List("v","c","s"), 1 )	

	
	
	
	// every 'recursive' call will produce another instance of the function !!! 
	def fill[T] :( Int, T, List[T] ) => List[T] = {
		
		( n :Int, t :T, list :List[T] ) => {
		
		  if( n <= 0 ) list else t :: fill( n-1, t, list ) // fill will produce a new instance !!!
	  }
	}
	
	trait  X[T]{
	
	  // since the polymorphic type is outsourced to the 'environment' (that is the trait)
	  // we can now directly define a polymorphic function (template) ...
	  val filll : ( Int, T, List[T] ) => List[T] = ( n :Int, t :T, list :List[T] ) => {
		
		  if( n <= 0 ) list else t :: filll( n-1, t, list ) 
	  }
	}
    // ... but to use it, we need to fix the type again (by making a concrete instance out of the trait, forcint us to do type parameterization)
	new X[Int]{}.filll
	
	// here we'll hide the creation of that trait behind a factory method
	def filll[T] = new X[T]{}.filll
	
	
	object Tuples{
		
		def first[A,_]( pair :(A,_) ) = pair._1
	}
  
	Tuples.first( ("1",2) )
	
	
  trait Filters[T]{
    val  filter = ( predicate :T => Boolean, xs :List[T] )  =>  {
      for(  x <- xs;  if predicate( x )  )  yield x
    }
  }
	
  object IntegerHandler extends Filters[Int]{
    val xs : List[Int] = filter( _ > 0, List( 1, -1, 2, -2 ) )
  }
  
  class StringHandler extends Object with Filters[String]{
	  val xs : List[String] = filter( _ startsWith( "A" ), List( "Anne", "George" ) )
  }
  
	
  def main( args :Array[String] ){
		
	println( "obj " + this.hashCode )
	  
	transform( 1, id[Int] )
		
	transform( "a", id[String] )
		
	transform( "x", new Id[String] )
		
	transform( (1,"x"), new Id[(Int,String)] )
		
	transform( (1,"x"), createIdFunc[(Int,String)] )

	val xs : List[String] = fill( 4, "x", Nil )
	println( xs )
	
	val ys : List[String] = fill( 4, "y", Nil )
	println( ys )
	
	val ones : List[Int] = fill( 5, 1, Nil )
	println( ones )
	
	val twos : List[Int] = filll( 5, 2, Nil )
	println( twos )
	
  }
}