package algebraic_datatypes

object Expression{

	def main( args :Array[String] ) = {
		
		println( "helloooo" )	
	}
	
	
	
	sealed abstract class Expression
	case class Literal( x :Int ) extends Expression
	case class Add( a :Expression, b :Expression ) extends Expression
	case class Mult( a :Expression, b :Expression ) extends Expression
	case class Sub( a :Expression, b :Expression ) extends Expression
	case class Mod( a :Expression, b :Expression ) extends Expression
	case class Pow( a :Expression, b :Expression ) extends Expression
	
	
	
	object SomeBinaryOp{
	  
	  def unapply(x: Any): Option[Tuple3[Expression,Expression,Expression]] = {
 	  
		x match{
	 	  
	 	  case add @ Add( l, r ) => Some( ( add, l, r ) )
	 	  case sub @ Sub( l, r ) => Some( ( sub, l, r ) )
	 	  case mult @ Mult( l, r ) => Some( ( mult, l, r ) )
	 	  case mod @ Mod( l, r ) => Some( ( mod, l, r ) )
	 	  case mod @ Pow( l, r ) => Some( ( mod, l, r ) )
	 	  case _ => None
	  	}
	  }  
	}
	
	
	val reduce : Expression => Expression
	    = 
        (exp : Expression ) =>  simplify( exp ) match {
	    //	(exp : Expression ) =>  ( exp ) match {

		case lit @ Literal( x ) => println("lit");lit

		case SomeBinaryOp( binOp,  left @ Literal(_), right @ Literal(_) ) => Literal( eval( binOp ) )

		case SomeBinaryOp( binOp, left @ Literal(_), rightExpr ) => build( binOp, left, reduce( rightExpr ) )
		case SomeBinaryOp( binOp, leftExpr, right @ Literal(_) ) => build( binOp, reduce( leftExpr ), right )
		case SomeBinaryOp( binOp, leftExpr, rightExpr ) => build( binOp, reduce( leftExpr ), rightExpr )
	 
	}	

	
	val simplify : Expression => Expression
		= 
		( expr :Expression) =>  { 
			
			//println( "simplify " + expr )
			
			val se = expr match {
		
			case SomeBinaryOp( Add(_,_),  Literal(0), expr ) => expr
			case SomeBinaryOp( Add(_,_),  expr, Literal(0) ) => expr
			
			case SomeBinaryOp( Mult(_,_),  expr, Literal(1) ) => expr
			case SomeBinaryOp( Mult(_,_),  Literal(1), expr ) => expr
			case SomeBinaryOp( Mult(_,_),  _, Literal(0) ) =>  Literal(0)
			case SomeBinaryOp( Mult(_,_),  Literal(0), _ ) => Literal(0)
		
			case SomeBinaryOp( Sub(_,_),  Literal(x), Literal(y) ) if x == y => Literal(0)
			
			case SomeBinaryOp( binOp,  left, right ) => build( binOp, simplify( left ), simplify( right ) )
			
			case _ => expr
		}
			
		println( expr + " simplified to " + se )
		
		se
	}
	     
	     
	
	
	val eval : Expression => Int 
	   =
	  _ match {
		
		case Literal( x ) => x
		case Add( leftExpr, rightExpr ) => eval( leftExpr ) + eval( rightExpr )
		case Mult( leftExpr, rightExpr ) => eval( leftExpr ) * eval( rightExpr )
		case Sub( leftExpr, rightExpr ) => eval( leftExpr ) - eval( rightExpr )
		case Mod( leftExpr, rightExpr ) => eval( leftExpr ) % eval( rightExpr )
		case Pow( leftExpr, rightExpr ) => power( eval( leftExpr ), eval( rightExpr ) )
	}

	val power : (Int,Int) => Int = ( base :Int, exp :Int ) => if( exp <= 1 ) base else base * power( base, exp - 1 )


	val build : (Expression, Expression, Expression ) => Expression
	   =
	  ( op :Expression, fst :Expression, snd :Expression ) => op match {
	 	  
	 	  case a :Add => Add( fst, snd )
	 	  case s :Sub => Sub( fst, snd )
	 	  case m :Mult => Mult( fst, snd )
	 	  case mo :Mod => Mod( fst, snd )
	 	  case p :Pow => Pow( fst, snd )
	  }
	
	

		
		val format : Expression => String
	   =
	  _ match {
		
		case Literal( x ) => x.toString
		case Add( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " + " + format( rightExpr ) + " )"
		case Mult( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " * " + format( rightExpr ) + " )"
		case Sub( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " - " + format( rightExpr ) + " )"
		case Mod( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " % " + format( rightExpr ) + " )"
		case Pow( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " ^ " + format( rightExpr ) + " )"
	}

		
	println( "exp 2 ................" )
	
		
	val expr_2 =  Mult( Literal( 6 ), Add( Literal( 3 ), Literal( 4 )  ) )
	
	println( "eval " + eval( expr_2 ) )
	println( format( expr_2 ) + " = " + eval( expr_2 ) )

	
	
	
	
	val expr1_2 = Mult( Add( Literal( 6 ), Mult(  Sub( Literal( 5 ), Literal( 3 ) ), Literal( 3 ) ) ), Add( Literal( 3 ), Mult(  Literal( 5 ),  Literal( 8 ) )  ) )
	
	println( format( expr1_2 ) + " = " + eval( expr1_2 ) )
	
	println( "goin to reduce ..." )
	
	val expr2_2 = reduce( expr1_2 )
	val expr3_2 = reduce( expr2_2 )
	val expr4_2 = reduce( expr3_2 )
	val expr5_2 = reduce( expr4_2 )
	val expr6_2 = reduce( expr5_2 )
	val expr7_2 = reduce( expr6_2 )
	
	println( format( expr1_2 ) )
	println( format( expr2_2 ) )
	println( format( expr3_2 ) )
	println( format( expr4_2 ) )
	println( format( expr5_2 ) )
	println( format( expr6_2 ) )
	println( format( expr7_2 ) )
	
	
	
	
	
	
	
	
	val resolve = (expr: Expression ) => stepResolve( expr, Nil ).reverse
		

	val stepResolve : ( Expression,List[Expression] ) => List[Expression] 
    = 
    ( expr :Expression, steps :List[Expression] ) => {
    	
    	//println( "step resolve " + expr )
    	
    	expr match {
			
        case Literal(_) => expr :: steps
        case _ => stepResolve( reduce( expr ), expr :: steps )
    	}
    }
	
    val expr = Mult( Add( Literal( 6 ), Mult(  Sub( Literal( 5 ), Mod( Literal(5), Add( Literal(2), Literal(1) ) ) ), Literal( 3 ) ) ), Add( Literal( 3 ), Mult(  Pow( Sub( Literal( 10 ), Literal( 6 )  ), Literal( 5 )),  Literal( 8 ) )  ) )
    
    println(  "mod : " + ( 5 % 3 ) )
	println( "resolved ..." )
	for( expr <- resolve( expr ) ) println( format( expr ) )
		
	
}