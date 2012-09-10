package typeclassopedia.show

object Instances {

	implicit object IntShow extends Show[Int]{
		
		override def show( n :Int ) = n.toString 
	}
}