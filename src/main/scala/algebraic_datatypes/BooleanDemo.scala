package algebraic_datatypes

object BooleanDemo extends Application{

	sealed abstract class Bool	
	case object True extends Bool{ override def toString = "True" }
	case object False extends Bool{ override def toString = "False" }
	
	val not : Bool => Bool 
		=
		( b : Bool ) => if( b eq True ) False else True
		
	val and : (Bool,Bool) => Bool
		=
		( a :Bool, b: Bool ) => if( a eq False ) False else b
		
	val or : (Bool,Bool) => Bool
		=
		( a :Bool, b: Bool ) => if( a eq True ) True else b	
		

println( "eval: " +  and( or( True, not( True ) ), not( False ) ) )

		def ma[A]( x:A ) = x match{
			
			case c:Bool => println( "BOOL!!!")
			case _	=> println( "DONT KNOW" )
		}
		
		ma( 2 )
		ma( True )
		
	// now using pattern matching
		
	val === : Bool => Bool => Bool
		=
		( a :Bool ) => ( b: Bool ) => ( a, b ) match {
			
			case ( True, True )   => True
			case ( False, False ) => True
			case _				  => False
		}
		
	val !!! : Bool => Bool 
		=
		( b : Bool ) => b match {
		
			case True  => False
			case False => True
		}
		
	val &&& : Bool => Bool => Bool
		=
		( a :Bool ) => ( b: Bool ) => a match {
			
			case False => False
			case _	   => b
		}
		
	val ||| : Bool => Bool => Bool
		=
		( a :Bool ) => ( b: Bool ) => a match{
			
			case True => True
			case _    => b
		}
		
	
		
	val count = ( x :Int, count :Bool ) => if( count eq True ) x + 1 else x
	
	
	val inCase = ( b :Bool ) => ( block :() => Unit ) => {
		
		if( b eq True ) block()
	}
	
	inCase( True )( () => println( "hello ..." ) )
	
	
	def inCase[A]( b: Bool, ifTrue : () => A, ifFalse : () => A  ) : A =
		b match {
		
		case True => ifTrue()
		case False => ifFalse()
	}
	

	println(  inCase( True, () => 1, () => 0 ) )
	println(  inCase( False, () => 1, () => 0 ) )
	
	val x :Any = inCase( and( True, True ), () => true, () => ""  )
	
	
	println( "t == f " + (True == False) )
	
		
	println( "True == False :" + (True == False) )
	println( "True eq False " + (True eq False) )
	println( "True == True " + (True == True) )
	println( "True eq True " + (True eq True) )
	println( "False == False " + (False == False) )
	println( "False eq False " + (False eq False) )
	
	println( "!!!" )
	
}