package realWorldHaskell.json

import utils.ListFunctions._
import utils.FuncUtils._
import utils.StringFunctions._

object JSON {
  
	def main(args : Array[String]) : Unit = {}
	
	// an algebraic datatype, representing all possible JSON-Type
	// one value constructor for each JSON-Type
	abstract trait JValue
	case class JString( txt :String ) extends JValue
	case class JNumber( num :Double ) extends JValue
	case class JBool( bool :Boolean ) extends JValue
	case object JNull extends JValue
	case class JObject( vals :List[(String,JValue)] ) extends JValue
	case class JArray( elems :List[JValue] ) extends JValue
	
	
	
	// a (general) pretty printing 'library' (not limited to prety printing JSON-Values)
	
	// first, another type for abstracting the result of a pretty-printing-process
	// so the result of pretty printing isn't just an ordinary, plain String but something of type Doc
	abstract trait Doc {
		// an 'infix' function for composing two Doc-Instances
		def <>( that :Doc ) :Doc = concat( this )( that )
	}
	case object DEmpty extends Doc
	case object DLine extends Doc
	case class DText( txt: String ) extends Doc	
	case class DChar( chr :Char ) extends Doc
	case class DConcat( d1 :Doc, d2 :Doc ) extends Doc
	case class DUnion( d1 :Doc, d2 :Doc ) extends Doc
	
	
	// some constructor functions for constructing Doc-Instances for text, doubles, chars, ...
	val empty  : Doc            = DEmpty
	val line   : Doc            = DLine
	val text   : String => Doc  =  s => if ( s == "" ) DEmpty else DText( s ) 
	val double : Double => Doc  =  d => text( d.toString )
	val char   : Char   => Doc  =  c => DChar( c )
	//val string : String => Doc  =  enclose( '"' )( '"' ) o hcat o map( oneChar ) o toCharList
	val string : String => Doc  =  s => DText( s )

	val concat : Doc => Doc => Doc  =  x => y => ( x, y ) match {	    	
	    case ( DEmpty, b )	 => b
	    case ( a, DEmpty )   => a
	    case ( a, b )        => DConcat( x, y )
	}
	
	// a function for enclosing / wrapping a Doc with some Characters (an opening and a closing character)
	val enclose : Char => Char => Doc => Doc
		=
		open => close => doc => char( open ) <> doc <> char( close )
	
	// folding a List of Docs, given a folding Function func of type ( Doc => Doc => Doc )
    // using foldr by partially applying the given function and an empty Doc (remaining the List to fold over as open)
	val fold : ( Doc => Doc => Doc ) => List[Doc] => Doc
	    =
	    func => foldr( func )( empty )

	// a function for concatenating a list of Doc-Instances into one single Doc-Instance
	val hcat : List[Doc] => Doc  =  fold( concat )
	    
	    
	// a 'Map' - mapping a Character to its escaped Character (for all characters for which an escaping is necessary)
	val simpleEscapes : List[(Char,String)]
		= {			
			val ch : Char => String => (Char,String)  =  a => b => ( a, "\\" + b ) 
			
			zipWith ( ch ) ( '\b' :: '\n' :: '\f' :: '\r' :: '\t' :: '\\' :: '\"' :: '/' :: Nil ) (  "b" ::  "n" ::  "f" ::  "r" ::  "t" :: "\\" :: "\"" :: "/" :: Nil )
		}
	
	// taking a character and mapping / converting it to a Doc, using the escaped character (if necessary to escape)	
	val oneChar : Char => Doc  
	    = 
		( c :Char ) => lookup( simpleEscapes )( c ) match {
			
			case Some( r )               => text( r )
			
			case _                       => char( c )
		}

	val flatten : Doc => Doc  =  _ match {
		
		case DConcat( x, y )  => concat( flatten( x ) )( flatten( y ) )
		case DLine            => char( ' ' )
		case DUnion( x, _ )   => flatten( x )
		case other            => other
	}
	
	val group : Doc => Doc  =  d => DUnion( flatten( d ), d )
	
	val softline : Doc  =  group( line )
	
	val </> : Doc => Doc => Doc  =  x => y =>  x <> softline <> y		
		
	// function that combines a list of Doc values into one, possibly wrapping lines if the 
	// output will not fit on a single line
	val fsep : List[Doc] => Doc  =  fold( </> )
	
	

	
	
	// function that 'shuffles' a Doc between every element of a list of Docs 
	val punctuate : Doc => List[Doc] => List[Doc]
	    =
	    doc => docs => ( doc ,docs ) match {
	    	
	    	case ( _, Nil )                        => Nil
	    	
	    	case ( _, singletonList @ (d :: Nil) ) => singletonList 
	    	
	    	case( p, d :: ds )                     => ( d <> p ) :: punctuate( p )( ds )
	    }
	    
	    
	    	
	val transform : List[Doc] => String  =  _ match {
	
	    case Nil	 => ""
	    
	    case d :: ds => d match {
	    			
	    					case DEmpty          => transform( ds )
	    					case DChar( c )      => c.toString + transform( ds )
	    					case DText( s )      => s + transform( ds )
	    					case DLine           => '\n' + transform( ds )
	    					case DConcat( a, b ) => transform( a :: b :: ds )
	    					case DUnion( _, b )  => transform( b :: ds )
	    				}
	}	    
	    
	// a function for compact rendering (e.g. for sending over a wire / network)
	// wrapping the doc in a list - the list is like a stack for Docs to process (gets filled with temporary docs
	// while processing the Stack of Docs (see function transform) - initially filled only with the 'whole' Doc to render
	val compact : Doc => String  =  doc => transform( doc :: Nil )
	
	    
	// ... and now a Render-Function, specifically for JSON-Objects, turning a JSON-Value
	//     into a neutral Doc-Representation	
	val renderJValue : JValue => Doc 
		=
		( jvalue : JValue ) => jvalue match {
			
			case JBool( true )   => text( "true" )
			case JBool( false )  => text( "false" )
			case JNull           => text( "null" )
			case JNumber( num )  => double( num )
			case JString( str )  => string( str )
			case JArray( elems ) => series( '[', ']' )( renderJValue )( elems )
			case JObject( vals ) => series( '{', '}' )( field )( vals )
		}
		
		
	val field : ( ( String,JValue ) ) => Doc  =  _ match {
		
		case ( name, jval )  =>  string( name ) <> text( ": " ) <> renderJValue( jval )
	}
			
		
	def series[A]( open: Char, close :Char )( item : A => Doc ) : List[A] => Doc  
	    =
	    enclose( open )( close ) o fsep o punctuate( char( ',' ) ) o map( item )
	
	
	// some samples
	
	println( "samples ..." )
	
	val json1 = JObject(
			
					( "name", JString( "Hans" ) ) ::
					( "alter", JNumber( 66 ) )    ::
					Nil
				)

	println( "compact: " + compact( renderJValue( json1 ) ) )
}
