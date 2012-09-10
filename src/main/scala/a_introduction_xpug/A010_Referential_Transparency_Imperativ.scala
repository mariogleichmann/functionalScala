package a_introduction_xpug

object A010_Referential_Transparency_Imperativ extends Application{

  
  
 var globalValue = 0;

 
 def rq( n :Int ) :Int = {
   
   globalValue = globalValue + 1;
  
   n + globalValue;
 }


  
  val x = 1
  val y = 2

  var p = rq(x) + rq(y) * ( rq(x) - rq(x) ) - rq(y);
  
  
  println( p )
  
}