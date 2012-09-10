package a_introduction_xpug

import A200_Monadic_Parser_Composition._
import A201_Item._
import A202_Choice._
import A203_Failure._
import A204_Deliver._
import A205_Monadic_Bind._

object A208_Multiples {

  
  def many[A] :Parser[A] => Parser[List[A]] =  p => either( many1( p ) ) or deliver( Nil )
  
  
  def many1[A] :Parser[A] => Parser[List[A]] = 
    
    p => p                     >>= ( v => 
         many( p )             >>= ( vs => 
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
 
}