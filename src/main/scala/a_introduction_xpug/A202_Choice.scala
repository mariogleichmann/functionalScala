package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A201_Item._

object A202_Choice {

  
  def choice[A] : Parser[A] => Parser[A] => Parser[A] 
    = 
    parser1 => parser2 =>  input => parser1( input ) match {
      
      								case PARSE_FAILURE => parser2( input )
      								
      								case result 	   => result    } 
    
    
    
    
    
  class ChoicePrepare[A]( parser1 :Parser[A] ){
      
      def or( parser2 :Parser[A] ) = choice( parser1 )( parser2 )
  }
  
  def either[A]( parser1 :Parser[A] ) = new ChoicePrepare( parser1 )			
    			

  
  
    def main( args :Array[String] ){
    
    
	  println( parse( either( item ) or item , "abc" ) )
    
  }
}