package algebraic_datatypes

object BooleanWithMaybe extends Application {

	sealed abstract class Bool	
	case object True extends Bool
	case object False extends Bool
	case object Maybe extends Bool
	
		
	// now using pattern matching
		
	val === : Bool => Bool => Bool
		=
		( a :Bool ) => ( b: Bool ) => ( a, b ) match {
			
			case ( True, True )   => True
			case ( False, False ) => True
			case ( Maybe, Maybe ) => True
			case _				  => False
		}
		
	val !!! : Bool => Bool 
		=
		( b : Bool ) => b match {
		
			case True  => False
			case False => True
			case Maybe => Maybe
		}
				
	val &&& : Bool => Bool => Bool
		=
		( a :Bool ) => ( b: Bool ) => (a,b) match {
			
			case ( False, _ ) => False
			case ( _, False ) => False
			case ( Maybe, _ ) => Maybe
			case ( _, Maybe ) => Maybe		
			case _	   		  => True			
		}
		
	val ||| : Bool => Bool => Bool
		=
		( a :Bool ) => ( b: Bool ) => (a,b) match{
			
			case ( True, _  ) | ( _, True  )  => True
			case ( Maybe, _ ) | ( _, Maybe )  => Maybe	
			case _	   		  				  => False
		}
		
	
		
	implicit def toTernary( b :Bool ) = new TernaryReceiveTrue( b )
	
	class TernaryReceiveTrue[+A]( b :Bool ){
		//def ?[A]( iftrue : => A ) = new TernaryReceiveFalse( b, iftrue )
		def ?[A]( iftrue : A ) = new TernaryReceiveFalse( b, iftrue )
	}
		
	//class TernaryReceiveFalse[+A]( b :Bool, iftrue : => A  ){
	class TernaryReceiveFalse[+A]( b :Bool, iftrue : A  ){
		//def |[B >: A]( iffalse : => B ) = new TernaryReceiveMaybe[B]( b, iftrue, iffalse )
		def |[B >: A]( iffalse : B ) = new TernaryReceiveMaybe[B]( b, iftrue, iffalse )
	}
		
	//class TernaryReceiveMaybe[+A]( b :Bool, iftrue : => A, iffalse : => A  ){
	class TernaryReceiveMaybe[+A]( b :Bool, iftrue : A, iffalse : A  ){
		//def |[B >: A]( ifmaybe : => B ) = b match {
		def |[B >: A]( ifmaybe : B ) = b match {
			case True  => iftrue
			case False => iffalse
			case Maybe => ifmaybe	
		}
	}
	
	
	val shouldBeTRUE = True ? "TRUE" | "FALSE" | "MAYBE"
	val shouldBeFALSE = False ? "TRUE" | "FALSE" | "MAYBE"
	val shouldBeMAYBE = Maybe ? "TRUE" | "FALSE" | "MAYBE"
	
	println( "shouldBeTRUE : " + shouldBeTRUE )
	println( "shouldBeFALSE : " + shouldBeFALSE )
	println( "shouldBeMAYBE : " + shouldBeMAYBE )
	
	class Person
	class Teacher extends Person
	class Pupil extends Person
	class Padawan extends Person
	class Padapadawan extends Padawan
	
	val shouldbePupil :Person = Maybe ? (new Teacher :Person) | (new Pupil :Person) | (new Padawan :Person)
	
	val shouldbePupil2 :Person = Maybe ? new Teacher  | new Padapadawan | new Padawan
}


//Moolean
//
//Now wait another minute. Didn't we say that pattern matching leads to well-structured, simple to read functions? 
//In the above example, we quite increased the functions body, compared to our first version, using an if-expression!
//Yep, there's a little extra effort for introducing the match expression, but that's amortized as soon as you want to consider some more complex cases. 
//For a first step

