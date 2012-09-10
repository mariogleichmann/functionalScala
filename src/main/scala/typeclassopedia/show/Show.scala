package typeclassopedia.show

trait Show[A] {

	def show( a :A ) : String
}

object showUtils{
	
	// an implicit conversion for an instance of typeclass Show to a 'RichShow'-Instance, so we could
	// call show in an OO-manner directly on the typeclass-Instance
	implicit def toRichShow[I : Show]( instance : I ) = new Object {
		
		val evidence = implicitly[Show[I]] 
		
		def show : String = evidence.show( instance )
	}
	
	// ... or a convenient way to call show on an arbitrary instance of the typeclass
	// hiding the boilerplate code of 'capturing' the implicit parameter 8whoch is the typeclass trait instance)
	def show[I :Show ]( i : I ) = implicitly[Show[I]].show( i )
	
}