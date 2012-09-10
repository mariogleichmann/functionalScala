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

object A230_ArithExpr_Parser extends Application{

  
  val factor :Parser[Int] 
    =     
    for( r <- either( for(  _  <- symbol( "(" ); 
   		 					e  <- expr;
   		 					_  <- symbol( ")" );
   		 					ir <- deliver( e ) ) yield ir )
    		  or natural  ) yield r
        

  val term :Parser[Int] 
    =
    for( f <- factor;
    	 r <- either ( for( _  <- symbol( "*" );
    	                    t  <- term;
    	                    ir <- deliver( f * t ) ) yield ir )
    	      or deliver( f ) ) yield r
  
 
  val expr :Parser[Int] 
    = 
    for( t <- term;
    	 r <- either ( for( _  <- symbol( "+" );
    	                    e  <- expr;
    	                    ir <- deliver( t + e ) ) yield ir )
    	      or deliver( t ) ) yield r         
     
    	      
    
  
  val eval :String => Int 
    =
    inp => parse( expr, inp ) match {
      case List( (n, "" ) )	=> n     
      case Nil 				=> error( "invalid input" )
      case List( (_, out) ) => error( "unused input " + out ) 
    }
    
    
  println( "parse (2+3)*4: " + parse( expr, "(2+3)*4" ) )
  println( "eval 2+3*4 = " + eval( "2+3*4" ) )
  println( "eval (2*5+(3+7))*3+4*( ((3))+(2*(1+4))) = " + eval( "(2*5+(3+7))*3+4*( ((3))+(2*(1+4)))" ) )
  
}