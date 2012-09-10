package a_introduction_xpug

object A200_Monadic_Parser_Composition {

  
  type Parser[A]  =  String => List[(A,String)]
  
  
  
  val PARSE_FAILURE = Nil
  
  
  
  def parse[A]( parser :Parser[A], input :String ) : List[(A,String)] = parser( input )
  
  
  


}