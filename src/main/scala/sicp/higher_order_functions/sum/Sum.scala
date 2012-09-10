package sicp.higher_order_functions.sum

object Sum extends Application{

  // recursive
  def sum( func: Double => Double, from :Double, to :Double, next :Double => Double  ) : Double = {
    
    if( from > to ) 0
    else            func( from ) + sum( func, next( from ), to, next )
  }
  
  
    def sum_tailrec( func: Double => Double, from :Double, to :Double, next :Double => Double  ) : Double = {
    
      def iter( acc :Double, start :Double ) : Double = {      
        
          //println( "iter( " + acc + ", " + start  )
        
    	  if( start > to ) acc
    	  else             iter( acc + func( start ), next( start ) )
      }
    	  
     iter( 0, from )
  }
  
  
  def inc( n :Double ) = n + 1
  
  def identity( n :Double ) = n
  
  
  def summeGanzeZahlen( from :Int, to :Int ) = sum_tailrec( identity, from, to, inc )
  
  println( "summe ganze zahlen 1 .. 100 : " + summeGanzeZahlen( 1, 5 ) )
  
  
  // ----
  
  def integral( func :Double => Double, a :Double, b :Double, dx :Double ) = {
    
    //def addDx( x :Double ) = x + dx
    
    sum_tailrec( func, a + (dx/2), b, x => x + dx ) * dx
  }
  
  
  def kubik( x :Double ) = x * x * x
  
  println( "integral kubik zwischen 0 und 1 : " + integral( kubik, 0, 1, 0.000001 ) )
  
}