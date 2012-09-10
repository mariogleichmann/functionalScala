package abstract_types

object UnionTypes {

    
  // union types - see http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/
  
  type NOT[A]  =  A => Nothing
  
  type NOTNOT[A] = NOT[NOT[A]]
  
  type V[T, U] = NOT[NOT[T] with NOT[U]]
  
  type OR[T, U] = { type LAMBDA[X] = NOTNOT[X] <:< (T V U) }
    
  def size[T: (Int OR String)#LAMBDA](t: T) = t match {
	    case i: Int => i
	    case s: String => s.length
	}
  
  
}