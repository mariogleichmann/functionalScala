package a_introduction_xpug

object A000_TotalSum_Imperative extends Application{

  
  var total = 0
  
  def sumUpTo( limit :Int ) :Int = {
    
         
    var i = 0
    
    
    while( i < limit ){
    	
      i += 1
      
      
      total = total + i 
      
     
      
    }
    
    return total
  }
  
  
  
  println( sumUpTo( 10 ) )  
  
  println( sumUpTo( 10 ) )
  
  
}