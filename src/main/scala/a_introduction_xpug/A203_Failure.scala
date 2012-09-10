package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A201_Item._
import A202_Choice._

object A203_Failure {

  
   def failure[A] :Parser[A]  =   x => PARSE_FAILURE
   
   
   
   
  def main( args :Array[String] ){
       
	  println( parse( failure, "abc" ) )
    
  }
}