package sicp.recursive_and_tail_recursive.add

import annotation.tailrec

object TailrecursiveVersion extends Application{

    def inc( x :Int ) = x + 1
    def dec( x :Int ) = x - 1
    
    @tailrec def add( a :Int, b :Int ) :Int = if( a == 0 ) b else add( dec( a ), inc( b ) )
}