package typeclassopedia.monad

import typeclassopedia.monad.Monad._

object Parser extends Application{

  type Parser[A] = String => List[(A,String)]
  
  def parse[A]( pa :Parser[A], inp :String ) : List[(A,String)] = pa( inp )
  
  val item :Parser[Char]  
      =  
      x => x.toList match {    
      					case Nil 		=> Nil
      					case y :: ys    => List( (y, ys.mkString ) )
      }
  
  def failure[A] :Parser[A] =   x => Nil
  
  def deliver[A] : A => Parser[A] =   x => ( inp => List( (x,inp) ) )
  
  def choice[A] : Parser[A] => Parser[A] => Parser[A] = 
    
    p1 => p2 => ( inp => p1( inp ) match {
      								case Nil => p2( inp )
      								case res => res
    			} )
    			
   
   // making choice to use as 'infix' ... 			
    			
    def either[A]( p1 :Parser[A] ) = new ChoicePrepare( p1 )			
    			
    class ChoicePrepare[A]( p1 :Parser[A] ){
      
      def or( p2 :Parser[A] ) = choice( p1 )( p2 )
    }
    
    // either( item ) or failure // sample
    			
  // ----------------
    
   
  class ParserUtils[A]( pa :Parser[A] ){
    
    def ->>>[B]( f :A => Parser[B] ) : Parser[B] = {
            		  
      inp =>  
        
        pa( inp ) match {
		    case Nil 			=> Nil
		    case (v,out) :: Nil => f( v )( out )
		}
    }
  }
  
  implicit def toRichParser[A]( pa :Parser[A] ) = new ParserUtils( pa )
    
  
  
  import typeclassopedia.monad.Samples._
  
  //def sat( p :Char => Boolean ) :Parser[Char] =  item ->>> ( x => if( p(x) ) deliver(x) else failure )
  def sat( p :Char => Boolean ) :Parser[Char] =  item >>= ( x => if( p(x) ) deliver(x) else failure )
  
  val digit = sat( _.isDigit )
  
  val alpha = sat( _.toString.matches("[A-Za-z]") )
  
  val char :Char => Parser[Char] = inp => sat( _ == inp  )
  
  val letter = sat( _.isLetter )
  
  val whitespace = sat( _.isWhitespace ) 
  
  val spaceChar = sat( _.isSpaceChar )
  
  val lower = sat( _.isLowerCase )
  
  val alphanum = either ( digit ) or ( alpha )
  
  // ...
   
  val charSeq :String => Parser[String] = 
    
    inp => inp.toList match {
      
				case Nil      => deliver( "" )
									
				case x :: xs  => char( x )    			>>= ( _ => 
							 	 charSeq( xs.mkString ) >>= ( _ => 
							 	 deliver( inp ) ) )
    		}
   

  def many[A] :Parser[A] => Parser[List[A]] =  p => either( many1( p ) ) or deliver( Nil )
    
  def many1[A] :Parser[A] => Parser[List[A]] = 
    
    p => p         >>= ( v => 
         many( p ) >>= ( vs => 
         deliver( v :: vs ) ) )
    
         
  
  def multiple[A] : Parser[A] => Parser[String] =
    
    p => ( inp => many( p )(inp) match {
      								case Nil           => Nil
      								case List( (v,s) ) => List( ( v.mkString, s ) )
    							} )
    							
  def multiple1[A] : Parser[A] => Parser[String] =
    
    p => ( inp => many1( p )(inp) match {
      								case Nil           => Nil
      								case List( (v,s) ) => List( ( v.mkString, s ) )
    							} )    							
         
         
  val digits :Parser[String] = multiple( digit )
         
  val alphanums :Parser[String] = multiple( alphanum )

  // identitfiers / variable-Name : lowercase followed by multiple alphanums
  val ident = for( x   <- lower;
  				   xs  <- alphanums;
  				   ret <- deliver( x + xs ) ) yield ret
   
    
   val nat :Parser[Int] = for( xs  <- multiple1( digit );
   							   ret <- deliver( xs.toInt ) ) yield ret 
   							   
   
   val space :Parser[Unit] = for( _   <- many( spaceChar );
                                  ret <- deliver( () ) ) yield ret
                                  
   def token[A]( p :Parser[A] ) :Parser[A] =
     
     for( _   <- space;
          v   <- p;
          _   <- space;
          ret <- deliver( v ) ) yield ret
             
          
   val identifier = token( ident )
   
   val natural = token( nat )
   
   val symbol :String => Parser[String] =  s => token( charSeq( s ) )
}