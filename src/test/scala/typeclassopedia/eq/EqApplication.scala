package typeclassopedia.eq

trait EqApplication {

	def isEquals[T]( t1 :T, t2 :T )( implicit equ :Eq[T] ) = equ.eq( t1)( t2 )
}