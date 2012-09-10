package sicp.recursive_and_tail_recursive.wurzel

object Newton_Iterationsverfahren extends Application{

  def quadrat( x :Double ) : Double = x * x
  
  def mittelwert( x :Double, y :Double ) : Double =  ( x + y ) / 2
  
  
  def wurzel( x :Double ) : Double = {
    
    def gutGenug( schaetzwert : Double ) : Boolean =  Math.abs( quadrat( schaetzwert ) - x ) < 0.000001
    
    def verbessern( schaetzwert : Double ) : Double = mittelwert( schaetzwert, x / schaetzwert )
    
	def wurzelIter( schaetzwert : Double ) : Double = {
    
    	if( gutGenug( schaetzwert ) ) schaetzwert else wurzelIter( verbessern( schaetzwert ) )
    }   
    
    wurzelIter( 1 )
  }
  

  println( "wurzel 2 : " + wurzel( 2 ) )
  
  println( "wurzel 4 : " + wurzel( 4 ) )
}