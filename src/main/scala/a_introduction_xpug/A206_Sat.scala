package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A201_Item._
import A202_Choice._
import A203_Failure._
import A204_Deliver._
import A205_Monadic_Bind._

object A206_Sat {

  
  val sat :( Char => Boolean ) => Parser[Char] 
    =  
    pred  =>  item >>= ( x => if( pred(x) ) deliver(x) else failure )
  
  
}