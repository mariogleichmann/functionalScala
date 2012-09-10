package implicits

object Implicits {

  implicit def toFunctionReceiver[A]( arg :A ) = new Object{
    def -<[B]( func :A => B ) : B = func( arg ) 
  }
  
  implicit def toFunctionReceiver2[A,B]( argPair :(A,B) ) = new Object{
    def -<[C]( func :(A,B) => C ) : C = func( argPair._1, argPair._2 ) 
    def -<[C]( func :A => B => C ) : C = func( argPair._1 )( argPair._2 ) 
  }
  
  implicit def infix[A]( fstArg :A ) = new Object{
	  def -<[B,C]( func : (A,B) => C  ) = new FctnRev( fstArg, func )
	  def -<[B,C]( func : A => B => C  ) = new FctnRevCurried( fstArg, func )
  } 
  class FctnRev[A,B,C]( fstArg :A, func : (A,B) => C ) extends Function1[B,C]{
    def >-( sndArg :B ) = apply( sndArg )   
    def apply( sndArg :B ) = func( fstArg, sndArg )
  }
  class FctnRevCurried[A,B,C]( fstArg :A, func : A => B => C ) extends Function1[B,C]{
    def >-( sndArg :B ) = apply( sndArg )   
    def apply( sndArg :B ) = func( fstArg)( sndArg )
  }
  
  def main(args: Array[String]) {
    
	val succ = (x:Int) => x+1
	val dec = (x:Int) => x-1
	val mult = (x:Int) => (y:Int) => x*y
	val mult2 = (x:Int,y:Int) => x*y
	
	def first[A,B] = ( pair :(A,B)) => pair._1
	
	//( (1,"x") ) -< first
	
	println( 2 -< succ -< mult(3) -< dec  )
	
	println( (2,3) -< mult  )
	
	println( 3 -<mult2>- 7 )
	
	val double : Int => Int = 2 -< mult2
	
	println( double( 8 ) )
	
  }
}