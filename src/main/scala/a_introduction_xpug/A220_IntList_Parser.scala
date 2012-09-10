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
import A210_Token_Symbols._

object A220_IntList_Parser {

  
  val intListParser = for( _  <- symbol( "List(" );
                           n  <- natural;
                           ns <- many( for( _   <- symbol( "," );      // ns is a List of ints, since parser 'natural' produces Int-values ...
                                             nat <- natural ) yield nat // ... and parser 'many'  produces a list of whatever the passed parser produces  
                                           );
                            _  <- symbol( ")" );
                            rs <- deliver( n :: ns ) ) yield rs
   
    
                            
  println( "parse( intListParser, 'List( 1 ,2  , 3,4  )' ) : " + parse( intListParser, "List( 1 ,2  , 3,4  )" ) )  
     
  
  val iList :List[Int] = parse( intListParser, "List( 1 ,2  , 3,4  )" ).head._1
}