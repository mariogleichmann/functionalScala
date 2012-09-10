package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A201_Item._
import A202_Choice._
import A203_Failure._
import A204_Deliver._
import A205_Monadic_Bind._
import A208_Multiples._
import A207_Simple_Sats._

object A209_Simple_Multiples {

  
   val digits :Parser[String] = multiple( digit )
         
  
   val alphanums :Parser[String] = multiple( alphanum )
   
    
   val nat :Parser[Int] = for( xs  <- multiple1( digit );
   							   ret <- deliver( xs.toInt ) ) yield ret 
   							   
   
   val space :Parser[Unit] = for( _   <- many( spaceChar );
                                  ret <- deliver( () ) ) yield ret
}