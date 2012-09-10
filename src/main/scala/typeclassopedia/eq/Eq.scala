package typeclassopedia.eq

trait Eq[-E]{
    
  def eq( e1 :E)( e2 :E ) : Boolean
  
  def notEq( e1 :E)( e2 :E ) : Boolean = !eq(e1)(e2) // a default implementation, based on eq
}

object eqUtils{

	implicit def toEq[ T :Eq ]( t :T ) = new Object {
 
		val evidence = implicitly[Eq[T]] 
 
		def equ[B<:T]( o :B ) = evidence.eq( t )( o )
		
		def notEqu [B<:T]( o :B ) = evidence.notEq( t )( o )
	}
}