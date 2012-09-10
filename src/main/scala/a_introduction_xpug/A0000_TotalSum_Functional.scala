package a_introduction_xpug

object A0000_TotalSum_Functional extends Application {

    
  val sumUpTo : Int => Int  
      =  
	  limit => 
	    
	    	if( limit == 0 ) 0
	    	
	    	else limit + sumUpTo( limit - 1 )
   
	  
	  
   
  println( sumUpTo( 10 ) ) 
  
  println( sumUpTo( 10 ) )
  
  println( sumUpTo( 10 ) )
  
}