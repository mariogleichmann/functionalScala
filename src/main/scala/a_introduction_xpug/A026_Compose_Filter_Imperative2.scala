package a_introduction_xpug

import java.util.ArrayList;

object A025_Compose_Filter_Imperative2 extends Application{

  def filterEven( xs :ArrayList[Int] ) :ArrayList[Int] = {
    
    var i = 0
    
    var toRemove = new ArrayList[Int]
    
    while( i < xs.size ){
    
      val elem = xs.get( i )
      
      if( elem % 2 != 0 )
      
      toRemove.add( elem )
      
      i = i + 1
    }
    
    xs.removeAll( toRemove )
    
    return xs
  }  

  
  def filterPositive( xs :ArrayList[Int] ) :ArrayList[Int] = {
    
    var i = 0
    
    var toRemove = new ArrayList[Int]
    
    while( i < xs.size ){
    
      val elem = xs.get( i )
      
      if( elem <= 0 )
      
      toRemove.add( elem )
      
      i = i + 1
    }
    
    xs.removeAll( toRemove )
    
    return xs
  } 
  
  
  var xs = new ArrayList[Int](){{ add(1);add(-2);add(3);add(-4);add(-5);add(6);add(7);add(8) }} 
  
  println( filterEven( xs )  )
  
  println( filterPositive( xs )  )
}