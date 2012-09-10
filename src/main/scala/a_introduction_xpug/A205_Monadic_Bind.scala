package a_introduction_xpug

import A200_Monadic_Parser_Composition._

object A205_Monadic_Bind {

  
  class ComposableParser[A]( parser :Parser[A] ){
    
    def >>= [B]( func :A => Parser[B] ) : Parser[B] = {
            		  
      inp =>  
        
        parser( inp ) match {
          
		    case PARSE_FAILURE 	           => PARSE_FAILURE
		    
		    case ( parsed, rest ) :: Nil   => func( parsed )( rest )
		}
    }
    
    
    
    
    
    
    
    
    
    
    
    
    def flatMap [B]( func :A => Parser[B] ) : Parser[B]  =  >>= ( func )
        
    def Return[A]( a :A ) :Parser[A] =   inp => List( (a,inp) )
        
    def map[B]( f: A => B ) : Parser[B] = flatMap { x => Return( f(x) ) }
		
	def foreach[B]( f: A=>B) :Unit = { map( f ) } 
  }
  
  
  
  
  implicit def toComposableParser[A]( parser :Parser[A] ) = new ComposableParser( parser )

  
}