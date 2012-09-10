package algebraic_datatypes

object Propositions extends Application {

	type Name = String
	type Names = List[Name]
	type Env = List[(Name,Boolean)]
	
	sealed abstract case class Proposition
	case class Var( n :Name ) extends Proposition
	case object F extends Proposition // false
	case object T extends Proposition // true
	case class Not( p :Proposition ) extends Proposition
	case class Or( p :Proposition, q :Proposition ) extends Proposition 
	case class And( p :Proposition, q :Proposition ) extends Proposition 
	
	
   val par : String => String = ( s :String ) => "(" + s + ")"
   
  val show : Proposition => String
  	  = _ match {
	  
	  case Var( name ) 	=> name
	  case F			=> "FALSE"
	  case T			=> "TRUE"
	  case Not( prop )  => par( "!" + show( prop )  )
	  case Or( p, q )	=> par( show( p ) + " || " + show( q ) )
	  case And( p, q )	=> par( show( p ) + " && " + show( q ) )
  }
  
  val names : Proposition => Names
  	  = _ match {
	  
	  case Var( n )		=> List( n )
	  case F			=> Nil
	  case T			=> Nil
	  case Not( p )		=> names( p )
	  case Or( p, q )	=> nub( names( p ) ::: names( q ) )
	  case And( p, q )	=> nub( names( p ) ::: names( q ) )
  }
  	  
  def nub[T]( xs :List[T] ) : List[T] = {
	  
	  def shift( y :T, ys :List[T] ) = if( !contains( y, ys ) ) y :: ys else ys
	  
	  def nub( from :List[T], to :List[T] ) : List[T] = from match {
	 	  
	 	  case Nil	=> to
	 	  case ( f :: fs ) => nub( fs, shift( f, to ) )
	  }
	   nub( xs, Nil )
  }
  
  def contains[T]( x :T, xs :List[T] ) : Boolean = xs match {
	  
	  case Nil 						=> false
	  case ( y :: ys ) if( x == y ) => true
	  case ( y :: ys )  			=> contains( x, ys )
  }
  
  val l : List[(Int,String)] = (1,"a")::(2,"b")::Nil
  for( (a,b) <- l ) yield b
  
  
  type Dictionary[A,B] = List[(A,B)]
  
  def lookup[A,B]( dict :Dictionary[A,B], key: A ) : B = {
	  
	  var the = ( xs: List[B] ) => xs match{ case (x::Nil) => x }
	   
	  the( for( (a,b) <- dict if a == key ) yield b )
  }

  
  val eval : Env => Proposition => Boolean
  	  =
  	  ( env :Env ) => ( p :Proposition ) => p match {
  		  
  		  case Var( n ) 	=> lookup( env, n )
  		  case F			=> false
  		  case T			=> true
  		  case Not( p )		=> !eval( env )( p )
  		  case Or( p, q )	=> eval( env )( p ) || eval( env )( q )
  		  case And( p, q )	=> eval( env )( p ) && eval( env )( q )		  
  	  }
  	
  val envs : Names => List[Env] 
      = _ match {
	  
	  case Nil 			=> List( Nil ) // a list with an empty environment in it !!!
	  case ( x :: xs )	=> for( es <- envs( xs );
	 		  				     e <- ( for( b <- List( true, false ) ) yield( x, b ) ) ) yield e :: es 
  }
  
  val existTrue : List[Boolean] => Boolean
  	  = _ match {
	  
  	  	case Nil 		 => false
  	  	case ( x :: xs ) => x || existTrue( xs )
  }
  
  val allTrue : List[Boolean] => Boolean
  	  = _ match {
	  
  	  	case Nil 		 => true
  	  	case ( x :: xs ) => x && allTrue( xs )
  }
  
  val statisfiable : Proposition => Boolean
      =
      ( p :Proposition ) => existTrue( for( env <- envs( names( p ) ) ) yield eval( env )( p ) )
  
 val tautologie : Proposition => Boolean
      =
      ( p :Proposition ) => allTrue( for( env <- envs( names( p ) ) ) yield eval( env )( p ) )


  val p1 = Or( And( Var( "a" ) , Var( "b" ) ), And( Not( Var( "a" ) ), Not( Var( "b" ) ) ) )

  println( names( p1 ) )
  println( show( p1 ) )
  
  println( eval( List( ("a",false),("b",false) ) )( p1 ) )
  
  println( envs( names( p1 ) ) )
  
  println( statisfiable( p1 ) )
  
  println( tautologie( p1 ) )

}