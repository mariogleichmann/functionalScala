package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A201_Item._
import A202_Choice._
import A203_Failure._
import A204_Deliver._
import A205_Monadic_Bind._
import A208_Multiples._
import A209_Simple_Multiples._
import A207_Simple_Sats._


object A210_Token_Symbols {

  
   def token[A]( p :Parser[A] ) :Parser[A] 
     =  
     for( _   <- space;
          v   <- p;
          _   <- space;
          ret <- deliver( v ) ) yield ret
             
          
   val ident = for( x   <- lower;
				    xs  <- alphanums;
				    ret <- deliver( x + xs ) ) yield ret   
  				   
  				   
   val identifier = token( ident )
   
   
   val natural = token( nat )
   
   
   val symbol :String => Parser[String] =  s => token( charSeq( s ) )
}