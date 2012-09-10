package algebraic_datatypes

object ExpressionsWithVariables {

	
		sealed abstract class Expression
		case class Literal( x :Int ) extends Expression
		case class Add( a :Expression, b :Expression ) extends Expression
		case class Mult( a :Expression, b :Expression ) extends Expression
		case class Sub(  a :Expression, b :Expression ) extends Expression
	
/*+*/	case class Var( x :String ) extends Expression

	
	
	
	object SomeBinaryOp{
	  
	  def unapply(x: Any): Option[Tuple3[Expression,Expression,Expression]] = {
 	  
		x match{
	 	  
	 	  case add @ Add( l, r ) => Some( ( add, l, r ) )
	 	  case sub @ Sub( l, r ) => Some( ( sub, l, r ) )
	 	  case mult @ Mult( l, r ) => Some( ( mult, l, r ) )
	 	  case _ => None
	  	}
	  }  
	}
		
	object SomeAtom{
		
	  def unapply(x: Any): Option[Expression] = {
 	  
		x match{
	 	  
	 	  case literal @ Literal(_) => Some( literal )
	 	  case variable @ Var(_) => Some( variable )
	 	  case _ => None
	  	}
	  }		
	}
	
	
	val reduce : (Expression, Environment) => Expression = (exp : Expression, env :Environment ) => {
		
		println( "reducing " + exp )
		
	 simplify( exp ) match {
			
case SomeAtom( variable @ Var(_) ) => Literal( eval( variable, env ) )

/*+*/	case SomeAtom( a ) => a 

case SomeBinaryOp( binOp, SomeAtom(variable @ Var(_)), rightExpr ) => build( binOp, reduce( variable,env ), rightExpr )
case SomeBinaryOp( binOp, leftExpr, SomeAtom(variable @ Var(_)) ) => build( binOp, leftExpr, reduce( variable, env ) )

		
/*+*/	case SomeBinaryOp( binOp, SomeAtom(_), SomeAtom(_) ) => Literal( eval( binOp, env ) )


/*+*/	case SomeBinaryOp( binOp, left @ SomeAtom(_), rightExpr ) => build( binOp, left, reduce( rightExpr, env ) )
/*+*/	case SomeBinaryOp( binOp, leftExpr, right @ SomeAtom(_) ) => build( binOp, reduce( leftExpr, env  ), right )

/*+*/	case SomeBinaryOp( binOp, leftExpr, rightExpr ) => build( binOp, reduce( leftExpr, env ), rightExpr )
	 }
	}	
	
		
	val simplify : Expression => Expression
		= 
		( expr :Expression) => expr match {
		
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
	     
	   
/*+*/type Environment = List[ ( String, Int ) ]
	
/*+*/val lookup : ( String, Environment ) => Int 
	   =
	  (  Key :String, env :Environment ) => env match {
	 	  	 	  
	   	case Nil 				=> 0
	   	case ( Key, i ) :: _   	=> i
	   	case  _ :: xs			=> lookup( Key, xs )
	  }
	
	
	
/*+*/ val eval : (Expression, Environment) => Int 
	   =
/*+*/ (exp :Expression, env: Environment) => exp match {
		
		case Literal( x ) => x
/*+*/	case Var( x ) => lookup( x, env )
/*+*/	case Add( leftExpr, rightExpr ) => eval( leftExpr, env ) + eval( rightExpr, env )
/*+*/	case Mult( leftExpr, rightExpr ) => eval( leftExpr, env ) * eval( rightExpr, env )
/*+*/	case Sub( leftExpr, rightExpr ) => eval( leftExpr, env ) - eval( rightExpr, env )
	}



	val build : (Expression, Expression, Expression ) => Expression
	   =
	  ( op :Expression, fst :Expression, snd :Expression ) => op match {
	 	  
	 	  case a :Add => Add( fst, snd )
	 	  case s :Sub => Sub( fst, snd )
	 	  case m :Mult => Mult( fst, snd )
	  }
	
	

		
	val format : Expression => String
	   =
	  _ match {
		
/*+*/	case Var( x ) => x
		case Literal( x ) => x.toString
		case Add( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " + " + format( rightExpr ) + " )"
		case Mult( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " * " + format( rightExpr ) + " )"
		case Sub( leftExpr, rightExpr ) => "( " + format( leftExpr ) + " - " + format( rightExpr ) + " )"
	}

		
	println( "exp 2 ................" )
	
	val env = ( "y", 8 ) :: ( "x", 1) :: Nil
		
	val expr_2 =  Mult( Literal( 6 ), Add( Literal( 3 ), Literal( 4 )  ) )
	
	println( "eval " + eval( expr_2, env ) )
	println( format( expr_2 ) + " = " + eval( expr_2, env ) )

	
	
	
	
	val expr1_2 = Mult( Add( Literal( 6 ), Mult(  Sub( Var( "x" ), Literal( 3 ) ), Literal( 3 ) ) ), Add( Literal( 3 ), Mult(  Literal( 5 ),  Var( "y" ) )  ) )
	
	println( format( expr1_2 ) + " = " + eval( expr1_2, env ) )
	
	println( "goin to reduce ..." )
	
	val expr2_2 = reduce( expr1_2, env )
	val expr3_2 = reduce( expr2_2, env )
	val expr4_2 = reduce( expr3_2, env )
	val expr5_2 = reduce( expr4_2, env )
	val expr6_2 = reduce( expr5_2, env )
	
	println( format( expr1_2 ) )
	println( format( expr2_2 ) )
	println( format( expr3_2 ) )
	println( format( expr4_2 ) )
	println( format( expr5_2 ) )
	println( format( expr6_2 ) )
	
	
	
	
	val resolve = (expr: Expression, env :Environment ) => stepResolve( expr, env, Nil ).reverse
			

	val stepResolve : ( Expression, Environment, List[Expression] ) => List[Expression] 
    = 
    ( expr :Expression, env :Environment, steps :List[Expression] ) => expr match {
			
        case Literal(_) => expr :: steps
        case _ => stepResolve( reduce( expr, env ), env, expr :: steps )  
    }	
	
	
    println("---   ---   ---")
    
    for( exp <- resolve( expr1_2,  ( "y", 8 ) :: ( "x", 1) :: Nil ) ){ println( format( exp ) ) }
	

    println("---   ---   ---")
    
    for( exp <- resolve( expr1_2,  ( "y", 42 ) :: ( "x", 17 ) :: Nil ) ){ println( format( exp ) ) }
    
    
	def main( args :Array[String] ) {
		println(" ... -> ")
	}
	
	
}