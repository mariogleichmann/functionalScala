package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A201_Item._
import A202_Choice._
import A203_Failure._

object A204_Deliver {

  
  def deliver[A] : A => Parser[A]  =   x => ( input => List( (x, input) ) )
  
  
  
  
  def main( args :Array[String] ){
       
	  println( parse( deliver( "z" ), "abc" ) )
    
  }
  
}