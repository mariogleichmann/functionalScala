package a_introduction_xpug

import A200_Monadic_Parser_Composition._

object A201_Item {

  
  val item :Parser[Char] 
    =
    x => x.toList match {
        
      case Nil 		=> PARSE_FAILURE
        
      case y :: ys  => List( (y, ys.mkString ) )
    }
  

  
  def main( args :Array[String] ){
    
    
	  println( parse( item, "abc" ) )
	      
  }
}