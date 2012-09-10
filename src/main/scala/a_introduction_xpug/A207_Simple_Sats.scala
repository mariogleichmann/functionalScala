package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A202_Choice._
import A206_Sat._
import A204_Deliver._
import A205_Monadic_Bind._

object A207_Simple_Sats {

  val digit = sat( _.isDigit )
  
  val alpha = sat( _.toString.matches( "[A-Za-z]" ) )
  
  val char :Char => Parser[Char] = inp => sat( _ == inp  )
  
  val letter = sat( _.isLetter )
  
  val whitespace = sat( _.isWhitespace ) 
  
  val spaceChar = sat( _.isSpaceChar )
  
  val lower = sat( _.isLowerCase )
  
  val alphanum = either ( digit ) or ( alpha )

  
  
  val charSeq :String => Parser[String] = 
    
    inp => inp.toList match {
      
				case Nil      => deliver( "" )
									
				case x :: xs  => char( x )    			>>= ( _ => 
							 	 charSeq( xs.mkString ) >>= ( _ => 
							 	 deliver( inp ) ) )
    	   }
  
}