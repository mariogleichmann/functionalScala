package sicp.recursive_and_tail_recursive.add

object RecursiveVersion {

  def inc( x :Int ) = x + 1
  def dec( x :Int ) = x - 1
  
  def add( a :Int, b :Int ) : Int = if( a == 0 ) b else inc( add( dec( a ), b )  )
}