package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A201_Item._
import A202_Choice._
import A203_Failure._
import A204_Deliver._

object A2033_IsChar_Native {

  
    val isChar : Char => Parser[Char] 
    =
    char => x => x.toList match {
        
      case Nil 					  => PARSE_FAILURE
        
      case y :: ys if y == char   => List( (y, ys.mkString ) )
      
      case _				      => PARSE_FAILURE        
    }
    
    
    
  def main( args :Array[String] ){
       
	  println( parse( either( isChar( 'b' ) ) or failure , "abc" ) )
	  
	  println( parse( either( isChar( 'b' ) ) or deliver( 'c' ) , "abc" ) )
    
  }
  
}