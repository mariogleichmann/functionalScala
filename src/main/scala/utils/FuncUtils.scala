package utils

object FuncUtils {

	class RichFunction[B,C]( f :B => C ){
		
		def o[A]( g : A => B ) : A => C  =  ( a :A ) => f ( g( a ) )
	}
	
	implicit def toRichFunction[B,C]( func : B => C )  =  new RichFunction( func )
}