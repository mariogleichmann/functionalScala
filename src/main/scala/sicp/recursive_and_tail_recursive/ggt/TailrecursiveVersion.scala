package sicp.recursive_and_tail_recursive.ggt

object TailrecursiveVersion extends Application {

  
  def ggt( a :Int, b :Int ) : Int = {
    
    println( "ggt("+a+", " +b+")" )
    
    if( b == 0 ) a else ggt( b, a % b )
  }
  
  println( "ggt( 20, 16 ) : " + ggt( 16, 20 ) )
  
  
}