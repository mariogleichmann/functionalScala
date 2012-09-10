package typeclassopedia.read

trait Read[A] {

	def read( s :String ) : A
}


object readUtils{

	def read[A :Read]( s :String ) : A = implicitly[Read[A]].read( s )
}