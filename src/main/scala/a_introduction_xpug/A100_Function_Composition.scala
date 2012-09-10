package a_introduction_xpug

object A100_Function_Composition {

    
    val parse = (sentence : String) => sentence.split( " " ).toList
    
    val filterLen =  ( predicate: Int => Boolean ) => ( xs :List[Int] ) => for( x <- xs; if predicate( x ) ) yield x 
    
    val wordLength = ( words :List[String] ) => for( w <- words ) yield w.length 

    val size = ( nums :List[_] ) => nums.size
    
    val even = ( i :Int ) => i % 2 == 0
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    val evenWordCountClassic = ( sentence :String ) => size( filterLen( even)( wordLength( parse( sentence ) ) ) )

    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    class FunctionEnhancer[B,C]( f: B => C ){
    
    	def o[A](g: A => B ) =  (x :A) => f( g( x ) ) 
    }
  
    implicit def toFunctionEnhancer[B,C]( f: B => C ) = new FunctionEnhancer( f )

    
    
    
    
    
    
    
    
    val evenWordCount = size o filterLen( even ) o wordLength o parse
    
       
    println( "#even Words: " + evenWordCount( "this sentence contains five even words" )  )
    

}