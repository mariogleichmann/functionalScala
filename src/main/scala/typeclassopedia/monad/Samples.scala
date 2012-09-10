package typeclassopedia.monad

import typeclassopedia.monad.Monad._

object Samples {

//	class MonadUtils[M[_] : Monad,A]( ma :M[A] ){
//		
//		val monadEvidence = implicitly[Monad[M]]
//		
//		def >>= [B]( f: A => M[B] ) : M[B] = monadEvidence.bind( ma )( f )
//		
//		//def Return[B]( b :B ) = monadEvidence.Return( b )
//		
//		
//		// the following methods are needed to enable Monads to participate in for-comprehensions
//		// (for using guards in for comprehensions, we also would need to define a filter method
//		def flatMap [B]( f: A => M[B] ) : M[B] = monadEvidence.bind( ma )( f )
//		
//		def map[B]( f: A => B ) : M[B] = flatMap { x => monadEvidence.Return( f(x) ) }
//		
//		def foreach[B]( f: A=>B) :Unit = { map( f) } 
//	}
//
//  // an implicit conversion for any type M[_], for which's a Monad typeclass implementation available
//  // (making that type M[_] an instance of typeclass Monad)
//  // wraps that instance into MonadUtils, which provides some convenient notational forms of calling the monad functions
//  implicit def toMonadUtils[M[_] : Monad,A]( ma :M[A] ) = {
//	  val monadEvidence = implicitly[Monad[M]]
//	  new MonadUtils[M,A]( ma )( monadEvidence )
//  }
//
//  
//  
//  def Return[M[_]:Monad,A]( a :A ) :M[A] = implicitly[Monad[M]].Return( a )
//  
  
  
  // some conveniance methods for creating some Option-instances for Some and None which are of (super-)type Option 
  // (and not of sub-type Some or None)
  def some[A]( a :A ) :Option[A] = Some(a)
  def none :Option[Nothing] = None
  
  // a simple 'function' we'll use in the upcoming samples of type Int => Int => Option[Int]
  // ... which we are currying to Int => Option[Int] and then going to use with monad 'bind' method
  def sub[A]( x:Int )( y:Int ) :Option[Int] = if( x > y ) Some( x-y ) else None
  
  // flipping arguments of a function
  def flip[A,B,C]( f :A=>B=>C ) : B=>A=>C = (b:B) => (a:A) => f( a )( b )
  
  
  val res1 = some( 10 ) >>= flip( sub )(7) >>= ( x => Some(x * 2) )
  
  val res2 = some( 10 ) >>= flip( sub )(15) >>= ( x => Some(x * 2) )
  
  val res3 = some(100) >>= ( x => Some( x.toString ) ) >>= ( y => Some( y.length ) )
  
  println( "res1 : " + res1 )
  println( "res2 : " + res2 )
  println( "res3 : " + res3 )
  
  // wow, even usable in a for-comprehension / do-notation like manner ...
  val sum =
	  some(100) >>= ( x =>  
	  some(200) >>= ( y => 
	  some(300) >>= ( z => 
  		Return[Option,Int]( x+y+z ) ) ) ) // here, we're leveraging closures, while refering to x, y and z
  		
  println( "sum : " + sum )
  	
  
  // another example, usinf List as the underlying monad
  
  def mult[A]( a:Int )(b: Int ) :List[Int] = List( a, b, a*b )	
  
  val listmul = List(1,20,300) >>= mult( 10 ) >>= mult( 2 )
  
  println( "listmul : " + listmul )
  
 
   val list =
	  List(1,2,3) >>= ( x =>  if( x < 2 ) Nil else 
	  List(1,2,3) >>= ( y => 
	  List(1,2,3) >>= ( z => 
  		Return[List,(Int,Int,Int)]( (x,y,z) ) ) ) )
  		
  println( "list : " + list )
  
  def cartesian[A]( lst :List[A] ) = lst >>= ( x => 
  									 lst >>= ( y => 
  									 Return[List,(A,A)]( (x,y) ) ) ) // instead of Return... you could simply write: List( (x,y) )
  
  println( "cartesian Int : " + cartesian( List(1,2,3) ) )
  println( "cartesian String : " + cartesian( List("a","b","c") ) )
  
  
  // monadic 'if'
  // the else-case will not be 'evaluated' if the condition is true (and vice versa)
  // => this is different to the applicative 'if', where both cases will always be evaluated 
  // (and may result to None, even the 'None-making-case' isn't selected by the condition!!!)
  def miffy[M[_] : Monad, A]( mb :M[Boolean] )( mt :M[A] )( mf :M[A] ) : M[A] = {
	  
	  mb >>= ( cond => if( cond ) mt else mf )
  }
  
   println( " miffy : " + miffy( some(true) )( some( "yep" ) )( none ) )
  
  
   // ------------------------------
   
   
   import typeclassopedia.monad.Parser._
   
   var p :Parser[(Char,Char)] =
    item >>= ( x => 
    item >>= ( _ => 
    item >>= ( z => 
    deliver( (x,z) ) ) ) )
   
    println( "parse abcde : " + p( "abcde" ) )
   
   var p2 :Parser[(Char,Char)] =
    item >>= ( x => 
    item >>= ( _ => 
    item >>= ( z => 
    Return[Parser,(Char,Char)]( (x,z) ) ) ) )
   
   println( "parse abcde : " + p2( "abcde" ) )
   
   println( "parse digit 123 : " + parse( digit, "123" ) )
   println( "parse digit abc : " + parse( digit, "abc" ) )
   
   println( "parse char 1 : " + parse( char( '1' ), "123" ) )
   println( "parse char a : " + parse( char( 'a' ), "abc" ) )
   println( "parse char x : " + parse( char( 'x' ), "abc" ) )
   
   println( "parse charSeq abc abcdef : " + parse( charSeq( "abc" ), "abcdef" ) )
   println( "parse charSeq abc ab1234 : " + parse( charSeq( "abc" ), "ab1234" ) )
   println( "parse charSeq abc a      : " + parse( charSeq( "abc" ), "a" ) )
   println( "parse charSeq abc ''     : " + parse( charSeq( "abc" ), "" ) )
   println( "parse charSeq a abcdef   : " + parse( charSeq( "a" ), "abcdef" ) )
   
   println( "parse many digit 123abc  : " + parse( many( digit ), "123abc" ) )
   println( "parse many digit 123 345  : " + parse( many( digit ), "123 345" ) )
  
   println( "parse digits 123abc  : " + parse( digits, "123abc" ) )
   println( "parse digits 123 345  : " + parse( digits, "123 345" ) )  
   
   println( "parse ident aValidVarName123  : " + parse( ident, "aValidVarName123" ) )
   println( "parse ident INValidVarName123  : " + parse( ident, "INValidVarName123" ) )
   
   println( "parse nat 123 * 2  : " + parse( nat, "123" ) + "" +  parse( nat, "123" ).head._1 * 2  )
   println( "parse nat 12AB3  : " + parse( nat, "12AB3" ) )
   println( "parse nat abc  : " + parse( nat, "abc" ) )
   println( "parse nat ab123cd  : " + parse( nat, "ab123cd" ) )
   
   println( "parse space '    abc'  : " + parse( space, "    abc" ) )
   println( "parse space 'abc'  : " + parse( space, "abc" ) )
   
   println( "parse ident '   aValidVarName123'  : " + parse( ident, "   aValidVarName123" ) )
   println( "parse identifier '   aValidVarName123'  : " + parse( identifier, "   aValidVarName123" ) )
   
   println( "parse nat '   123'  : " + parse( nat, "   123" ) )
   println( "parse natural '   123'  : " + parse( natural, "   123" ) )

   
   // sample: a parser for a non-empty list of ints with arbitrary space
   
   val intListParser = for( _  <- symbol( "List(" );
                            n  <- natural;
                            ns <- many( for( _   <- symbol( "," );      // ns is a List of ints, since parser 'natural' produces Int-values ...
                                             nat <- natural ) yield nat // ... and parser 'many'  produces a list of whatever the passed parser produces  
                                           );
                            _  <- symbol( ")" );
                            rs <- deliver( n :: ns ) ) yield rs
   
  println( "parse( intListParser, 'List( 1 ,2  , 3,4  )' ) : " + parse( intListParser, "List( 1 ,2  , 3,4  )" ) )  
  val iList :List[Int] = parse( intListParser, "List( 1 ,2  , 3,4  )" ).head._1
    
   
  // sample: parsing simple arithmetic expressions
  	       
  val factor :Parser[Int] = {    
    for( r <- either( for(  _  <- symbol( "(" ); 
   		 					e  <- expr;
   		 					_  <- symbol( ")" );
   		 					ir <- deliver( e ) ) yield ir )
    		  or natural  ) yield r
    }    

  val term :Parser[Int] =
    for( f <- factor;
    	 r <- either ( for( _  <- symbol( "*" );
    	                    t  <- term;
    	                    ir <- deliver( f * t ) ) yield ir )
    	      or deliver( f ) ) yield r
  
 
  val expr :Parser[Int] = 
    for( t <- term;
    	 r <- either ( for( _  <- symbol( "+" );
    	                    e  <- expr;
    	                    ir <- deliver( t + e ) ) yield ir )
    	      or deliver( t ) ) yield r         
     
    	      
    
  
  val eval :String => Int =
    inp => parse( expr, inp ) match {
      case List( (n, "" ) )	=> n     
      case Nil 				=> error( "invalid input" )
      case List( (_, out) ) => error( "unused input " + out ) 
    }
    
    
  println( "eee : " + parse( expr, "(2+3)*4" ) )
  println( "eval 2+3*4 = " + eval( "2+3*4" ) )
  println( "eval (2*5+(3+7))*3+4*( ((3))+(2*(1+4))) = " + eval( "(2*5+(3+7))*3+4*( ((3))+(2*(1+4)))" ) )
  
  
  // wow, now i can do for-comprehensions, thanks to flatMap, map and foreach in MonadUtils (in which 
  // Parsers gets implicitly converted - as long as there's an implementation provided for the Monad typeclass)

  println( "parse comprehension : " + parse(  for( x   <- item;
  												   _   <- item;
  												   y   <- item;
                                                   res <- deliver( (x,y) ) ) yield res, "abcde" ) ) 
   
   
  def main( args :Array[String] ){	  
  }
    
}