package a_introduction_xpug

object A011_Referential_Transparency_Functional extends Application{

  
   def rq( n :Int ) :Int = {
    
       n + 1;
   }


  
  val x = 1
  val y = 2

  var p = 2 + 3 * ( rq(x) - rq(x) );
  
  
  println( p )
}