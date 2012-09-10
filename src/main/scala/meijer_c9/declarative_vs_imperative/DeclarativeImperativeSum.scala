package meijer_c9.declarative_vs_imperative

object DeclarativeImperativeSum extends Application{

  // imperative
  
  var total = 0
  var i = 0
  
  while( i < 10 ){
    
    i += 1
    total = total + i
  }
  
  println( total )
  
  // deklarative
  
  def sum( xs :Seq[Int] ) :Int = if( xs.isEmpty ) 0 else xs.head + sum( xs.tail )
  
  def product( xs :Seq[Int] ) :Int = if( xs.isEmpty ) 0 else xs.head * sum( xs.tail )
  
  // -> each expression (sum/product 1 to 10/1 to 20) can be varified individually!!!
  println( sum( 1 to 10 ) )
  println( sum( 1 to 20 ) )
  println( product( 1 to 10 ) )
  println( product( 1 to 20 ) )  
  
  
  
  def factorial( n :Int ) = product( 1 to n )
  
  
  
  def length( xs :Seq[Int] ) :Int = if( xs.isEmpty ) 0 else 1 + length( xs.tail )
  
  def average( xs :Seq[Int] ) :Double = sum( xs ) / length( xs ) 
  
  
  println( average( List( 1, 2, 3, 4, 5, 6, 7, 8 ) ) )
  
}