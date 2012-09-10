package algebraic_datatypes

object SomeExamples extends Application {

	/* algebraic datatypes 
	
	- bunch of alternatives
	
	 
	 
	
	 
	 */
	
	
	
	// data Season = Winter | Spring | Summer | Fall
	sealed abstract case class Season // this is the data type
	// and here are the constructors of the - the different values, belonging to the type ... and NO MORE ELSE !!!
	case object Winter extends Season
	case object Spring extends Season
	case object Summer extends Season
	case object Fall extends Season
	
	val next : Season => Season
		=
		( s :Season ) => 
			if( s eq Winter ) Spring else
			if( s eq Spring ) Summer else
			if( s eq Summer ) Fall else 
							  Winter
							  
	// constructors can be used during pattern matching on the 'left side' in Haskell (in Scala, any type can)
	val nextt : Season => Season
		= _ match {
		
		case Winter => Spring
		case Spring => Summer
		case Summer => Fall
		case Fall	=> Winter
	}
	
	val toInt : Season => Int 
		=
		( s :Season ) => 
			if( s eq Winter ) 0 else
			if( s eq Spring ) 1 else
			if( s eq Summer ) 2 else 
							  3
	
    val fromInt : Int => Season
    	= 
    	( i :Int ) =>
			if( i == 0 ) Winter else
			if( i == 1 ) Spring else
			if( i == 2 ) Summer else 
						 Fall
    		
	val nexttt : Season => Season
		=
		( s :Season) => fromInt( ( toInt( s ) + 1 ) % 4 )
	
	val eqSeason : Season => Season => Boolean
		=
		( a :Season ) => ( b :Season ) => toInt( a ) == toInt( b )
	
		
   lazy val seasons: Stream[Season] = Spring #:: Summer #:: Fall #:: Winter #:: seasons
  
   def nextSeason(now: Season): Season = { 
     def getNextFromStream( s: Stream[Season]): Season = s match {
       case x #:: ( tail@ y #:: rest ) => if (now eq x) y else getNextFromStream( tail )
     }   
    getNextFromStream(seasons)
   }
		
   def nextSeason2( Now: Season ): Season = { 
     def getNextFromStream( s: Stream[Season]): Season = s match {
       case Now #:: y #:: _ => y 
       case _				=> getNextFromStream( s tail )
     }   
    getNextFromStream(seasons)
   }

  println( "nextS2 Summer " + nextSeason2( Summer ) )		
  println( "nextS2 Spring " + nextSeason2( Spring ) )
  println( "nextS2 Winter " + nextSeason2( Winter ) )
  println( "nextS2 Fall " + nextSeason2( Fall ) )
  println( "nextS2 Summer " + nextSeason2( Summer ) )		
  println( "nextS2 Spring " + nextSeason2( Spring ) )
  println( "nextS2 Winter " + nextSeason2( Winter ) )
  println( "nextS2 Fall " + nextSeason2( Fall ) )
	
  // -------------------------------- 
  
  // color as a sum type (all values of that type are expressed through the sum (disjoint set) of all data constructors
  sealed abstract class Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
  
  // color as a product type (all values of that type are expressed as the 'product' of all different combinations of 
  // red, green and blue fields
  // the parameter types of the constructor are the factors of the product type
  sealed abstract class RGBColor
  case class AdditiveRGB( red :Int, green :Int, blue :Int )
  
  // ... or simply
  sealed case class RGBColorV2( red :Int, green :Int, blue :Int )
  
  // --------------------------------
  
	//data Shape = Circle Float | Rectangle Float Float
    // each of whose values is data from other datatypes wrapped in one of the constructors of the datatype
	sealed abstract class Shape
	case class Circle( radius :Double ) extends Shape // constructors can have arguments
	case class Rectangle( width :Double, height :Double ) extends Shape
	
	Rectangle( width = 2.5F, height = 1.5F )
	
	type Width = Double
	type Height = Double
	case class Rectangle2( w :Width, h :Height ) extends Shape
	case class Triangle(base: Double, height: Double) extends Shape

	
	
	val v1 : Width = 2.5F
	val v2 : Height = 3.5F
	val rec = Rectangle2( v2, v1 ) 
	
	Rectangle2( 1.1F :Height, 1.2F :Width )
	
	// baaaah - should produce compile error, since we feed a value of type Height to first constructor arg, which is of type Width
	// same goes for 2nd constructor argument
	
	import scala.Math.{Pi,pow}
	val area : Shape => Double
		= _ match {		
		case Circle( radius ) 			 => Pi * ( pow( radius, 2.0 ) )
				
		case Rectangle( 1, height )	 => height
		case Rectangle( width, 1 )	 => width
		
		case Rectangle( width, height )	 => width * height
		case Rectangle2( width, height ) => width * height
		
		case Triangle( 0, _ ) 	 => 0
		case Triangle( base, height ) 	 => height * base / 2
	}
	
	val topple : Rectangle => Rectangle
		=
	( rectangle : Rectangle ) => rectangle match {
		
		case Rectangle( w, h ) if h > w => Rectangle( h, w )
		case _ => rectangle
	}
	

	val switch : Rectangle => Rectangle
		=
	_ match {
		
		case Rectangle( w, h ) => Rectangle( h, w )
	}	
	
	case class Stack( top :Rectangle, bottom :Rectangle ) extends Shape
	
	
	
	
	
	
	val stack : ( Rectangle, Rectangle ) => Stack 
		=
	( r1 : Rectangle, r2 : Rectangle ) => ( r1, r2 ) match {
		
		//case( Rectangle( w1,h1 ), Rectangle( w2,h2 ) ) if w1 == w2 => Stack( r1, r2 )
		case( Rectangle( w1,_ ), Rectangle( _,h2 ) ) if w1 == h2 => stack( r1, switch( r2 ) )  
		case( Rectangle( _,h1 ), Rectangle( w2,_ ) ) if h1 == w2 => stack( switch( r1 ), r2 )
		case _ => Stack( r1, r2 )
	}

	
	
	
	
	val scale : ( Int, Int, Shape ) => Shape
		=
	( x :Int, maxArea :Int, shape :Shape ) => shape match{
		
		case Circle( r ) if area( Circle(r*x) ) < maxArea => Circle(r*x)	
		case Rectangle( w, h ) if area( Rectangle(w*x, h*x) ) < maxArea  => Rectangle(w*x, h*x)
		case Triangle( b, h ) if area( Triangle(b*x, h*x) ) < maxArea  => Triangle(b*x, h*x)
		case _ => shape
	}
	
	val fits : ( Shape, Shape ) => Boolean
		=
	( nested :Shape, host :Shape ) => ( nested, host ) match {
		
		case( rec @ Rectangle( w, _ ), tri @ Triangle( b, _ ) ) if( b == w ) => area( tri ) < area( rec )  
		
		
	}
	
	def scaleD[A <: Shape]( x :Int, shape :A ) = {
		
		val MAX_AREA = 100
		
		shape match{

			case Circle( r ) if area( Circle(r*x) ) < MAX_AREA => Circle(r*x)	
			case Rectangle( w, h ) if area( Rectangle(w*x, h*x) ) < MAX_AREA  => Rectangle(w*x, h*x)
			case Triangle( b, h ) if area( Triangle(b*x, h*x) ) < MAX_AREA  => Triangle(b*x, h*x)
			case _ => shape
		}
	}
	
	def scale2[A <: Shape]( times :Int, shape :A ) : A = shape match {
		case Circle( x ) => Circle( times * x ).asInstanceOf[A]
		case Rectangle( w, h ) => Rectangle( times * w, times * h ).asInstanceOf[A]
	}

	var re :Rectangle = scale2( 2, Rectangle( 10, 15 ) )
	
	val r :Shape = scaleD( 2, Rectangle( 10, 15 ) ) 
	
	
	println( area( Rectangle( 3, 4 ) ) )
	println( area( Rectangle( 1, 4 ) ) )
	
	
	// adding Position to shapes
	
	sealed abstract class PositionedShape
	case class PosCircle( x:Float, y:Float, radius :Double ) extends PositionedShape // constructors can have arguments
	case class PosRectangle( x:Float, y:Float, width :Double, height :Double ) extends PositionedShape
	
	// nested types: using another datatype for representing a position ...
	case class Point( x :Int, y :Int )
	
	// ... and using this one as a nested type within the value constructors of another datatype
	case class PosCircleV2( center :Point, radius :Double ) extends PositionedShape // constructors can have arguments
	case class PosRectangleV2( leftTopAngle :Point, width :Double, height :Double ) extends PositionedShape
	case class PosTriangle( apex :Point, base :Double, height :Double  ) extends PositionedShape
	
	
	
	
	
	
	
	// --------------------

	//data Exp = Lit Int | Add Exp Exp | Mul Exp Exp
	sealed abstract case class Expression
	case class Literal( x :Int ) extends Expression
	case class Add( x :Expression, y :Expression ) extends Expression
	case class Mult( x :Expression, y :Expression ) extends Expression
	case class Sub(  x :Expression, y :Expression ) extends Expression
	case class Mod( x :Expression, y :Expression ) extends Expression
	
	
	
	val eval : Expression => Int 
	   =
	  _ match {
		
		case Literal( x ) => x
		case Add( leftExpr, rightExpr ) => eval( leftExpr ) + eval( rightExpr )
		case Mult( leftExpr, rightExpr ) => eval( leftExpr ) * eval( rightExpr )
		case Sub( leftExpr, rightExpr ) => eval( leftExpr ) - eval( rightExpr )
	    case Mod( leftExpr, rightExpr ) => eval( leftExpr ) % eval( rightExpr )	
	}
	
	val reduce : Expression => Expression
        =
    (expr :Expression) => simplify( expr ) match {
		
		case lit @ Literal( _ ) => lit 
		
		case Add( Literal(_), Literal(_) ) => Literal( eval(expr) )
		case Add( left @ Literal(_), rightExpr ) => Add( left, reduce( rightExpr ) )
		case Add( leftExpr, right @ Literal(_) ) => Add( reduce( leftExpr ), right )
		case Add( leftExpr, rightExpr ) => Add( reduce( leftExpr ), rightExpr )
		
		case Sub( Literal(_), Literal(_) ) => Literal( eval(expr) )
		case Sub( left @ Literal(_), rightExpr ) => Sub( left, reduce( rightExpr ) )
		case Sub( leftExpr, right @ Literal(_) ) => Sub( reduce( leftExpr ), right )
		case Sub( leftExpr, rightExpr ) => Sub( reduce( leftExpr ), rightExpr )
		
		case Mult( Literal(_), Literal(_) ) => Literal( eval(expr) )
		case Mult( left @ Literal(_), rightExpr ) => Mult( left, reduce( rightExpr ) )
		case Mult( leftExpr, right @ Literal(_) ) => Mult( reduce( leftExpr ), right )
		case Mult( leftExpr, rightExpr ) => Mult( reduce( leftExpr ), rightExpr )
		
		case Mod( Literal(_), Literal(_) ) => Literal( eval(expr) )
		case Mod( left @ Literal(_), rightExpr ) => Mod( left, reduce( rightExpr ) )
		case Mod( leftExpr, right @ Literal(_) ) => Mod( reduce( leftExpr ), right )
		case Mod( leftExpr, rightExpr ) => Mod( reduce( leftExpr ), rightExpr )
	}
	
// simplifying ( 0 * ( 3 + ( 5 * 8 ) ) )
	val simplify : Expression => Expression
		= 
		( expr :Expression) => {
			
			println( "simplifying " + expr )
			
			expr match {
		
			case Add( Literal(0), expr ) => expr
			case Add( expr, Literal(0) ) => expr
			case Add( left, right ) => Add( simplify( left ), simplify( right ) )
			
			case Mult( Literal(0), _ ) => println( "mult 0 _ " ); Literal(0)
			case Mult( _, Literal(0) ) => Literal(0)
			case Mult( Literal(1), expr ) => expr
			case Mult( expr, Literal(1) ) => expr
			case Mult( left, right ) => Mult( simplify( left ), simplify( right ) )
			
			case Sub( Literal(x), Literal(y) ) if x == y => Literal(0)
			case Sub( left, right ) => Sub( simplify( left ), simplify( right ) )
			
			case _ => println( "no simplification" ); expr
		}
	}
	
	val trivialReduce = ( expr :Expression ) => reduce( simplify( expr ) )
	
	
	val format : Expression => String
	   =
	  _ match {
		
		case Literal( x ) => x.toString
		case Add( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " + " + format( rightExpr ) + " )"
		case Mult( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " * " + format( rightExpr ) + " )"
		case Sub( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " - " + format( rightExpr ) + " )"
	}
		
	val expr =  Mult( Literal( 6 ), Add( Literal( 3 ), Literal( 4 )  ) )
	
	println( "eval " + eval( expr ) )
	println( format( expr ) + " = " + eval( expr ) )
	
	
	val expr1_x =  Mult( Literal( 6 ), Add( Literal( 3 ), Literal( 4 )  ) )
	val expr2_x = Sub( Mult( Literal( 21 ), Add( Literal( 103 ), Literal( 42 ) ) ), Add( Literal( 17 ), Mult( Literal( 29 ), Literal( 7 ) ) ) )

	val expr1 = Mult( Sub( Literal( 6 ), Mult(  Sub( Literal( 5 ), Literal( 3 ) ), Literal( 3 ) ) ), Add( Literal( 3 ), Mult(  Literal( 5 ),  Literal( 8 ) )  ) )
	
	
	println( "expr1_x : " + format( expr1_x ) + " = " + eval( expr1_x ) )
	println( "expr2_x : "  + format( expr2_x ) + " = " + eval( expr2_x ) )
	println( "expr1 : "  + format( expr1 ) + " = " + eval( expr1 ) )
	
	
	
	
	val expr2 = reduce( expr1 )
	val expr3 = reduce( expr2 )
	val expr4 = reduce( expr3 )
	val expr5 = reduce( expr4 )
	val expr6 = reduce( expr5 )
	
	println( format( expr1 ) )
	println( format( expr2 ) )
	println( format( expr3 ) )
	println( format( expr4 ) )
	println( format( expr5 ) )
	println( format( expr6 ) )
	
//	val resolve : Expression => Sequence[Expression]
//	 =
//	( expr : Expression ) => {
//		
//		stepResolve( expr, Nil ).reverse
//	}
	
	val resolve = (expr: Expression ) => stepResolve( expr, Nil ).reverse
		
//	val step : ( Expression,List[Expression] ) => List[Expression] = ( e :Expression, steps :List[Expression] ) => { 
//		
//			//val re = reduce( simplify( e ) )
//			val re = reduce( e )
//			
//			re match {
//			
//				case Literal(_) => re :: steps
//				case _ => step( re, re :: steps )  
//		  }
//	}		

	val stepResolve : ( Expression,List[Expression] ) => List[Expression] 
    = 
    ( expr :Expression, steps :List[Expression] ) => {
    	
    	println( "step resolve " + expr )
    	
    	expr match {
			
        case Literal(_) => expr :: steps
        case _ => stepResolve( reduce( expr ), expr :: steps )
    	}
    }
	
	println( "resolved ..." )
	for( expr <- resolve( expr1 ) ) println( format( expr ) )
	
	
	
	
	
	
	
	// --------------------------------
	
	//data List a = Nil | Cons a (List a)
	sealed abstract case class LList[+T]
	case object NNil extends LList[Nothing]
	case class CCons[T]( x :T, tail :LList[T] ) extends LList[T]
	// cons -> constructive list - out of head of list and its tail

	def append[A]( xs :LList[A], ys :LList[A] ) : LList[A] = xs match {
		
		case NNil			=> ys
		case ( z CCons zs ) => CCons( z, append( zs, ys ) )
	}
	
	
	// ----------------------------
	
	
	//data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)
	sealed abstract case class Tree[+A]
	case object EmptyTree extends Tree[Nothing]
	case class Leaf[A]( a :A ) extends Tree[A]
	case class Branch[A]( l :Tree[A], r :Tree[A] ) extends Tree[A]
	
	// number of leafs
	val size : Tree[_] => Int
		= _ match{
		
		case EmptyTree 			=> 0
		case Leaf( _ ) 			=> 1
		case Branch( lt, rt )	=> size( lt ) + size( rt )
	}
	
	
	// ---------------------

	//data Maybe a = Nothing | Just a
	sealed abstract case class Maybe[A]
	case object Noothing extends Maybe
	case class Just[A]( a :A ) extends Maybe[A]
	
	
	// ------------------------------
	

	//data Pair a b = Pair a b
	sealed case class Pair[A,B]( a :A, b :B )

	//data Sum a b = Left a | Right b
	sealed abstract case class Sum[A,B]
	case class Left[A]( a :A ) extends Sum[A,Nothing]
	case class Right[B]( b :B ) extends Sum[Nothing,B]

	//data Nat = Zero | Succ Nat
	sealed abstract case class Nat
	case object Zero extends Nat
	case class Succ( n :Nat ) extends Nat
}