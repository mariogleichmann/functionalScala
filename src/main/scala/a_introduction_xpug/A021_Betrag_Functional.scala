package a_introduction_xpug

object A021_Betrag_Functional extends Application{

  
  class Betrag( var value :Int, currency :String ){
    
    def +( other :Betrag ) = new Betrag( this.value + other.value, currency )
    
    def -( other :Betrag ) = new Betrag( this.value - other.value, currency )
    
    def ==( other :Betrag ) =  this.value == other.value   
    
    override def toString =  value + " " + currency
  }
  
  
  var oneEur = new Betrag( 1, "EUR" );
  var twoEur = new Betrag( 2, "EUR" );
  var threeEur = new Betrag( 3, "EUR" );

  var sum = oneEur + ( twoEur + threeEur )   

  var difference = threeEur - ( twoEur + oneEur )  
  
  
  println( sum )
  
  println( difference )
}