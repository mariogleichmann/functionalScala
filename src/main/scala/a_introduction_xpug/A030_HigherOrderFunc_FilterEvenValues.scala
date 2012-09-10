package a_introduction_xpug


object A030_HigherOrderFunc_FilterEvenValues extends Application{

  case class Betrag( var value :Int, currency :String ){    
    def +( other :Betrag ) = new Betrag( this.value + other.value, currency )    
    def -( other :Betrag ) = new Betrag( this.value - other.value, currency )   
    def unary_- = new Betrag( -this.value, currency )
    def ==( other :Betrag ) =  this.value == other.value       
    override def toString =  value + " " + currency
    def <( other :Betrag ) = this.value < other.value
    def >( other :Betrag ) = this.value > other.value
  }
  
  val ZERO_EUR = Betrag( 0, "EUR" )
  
  val betraege = List( Betrag(10,"EUR"),Betrag(-21,"EUR"),Betrag(0,"EUR"),Betrag(30,"EUR"),Betrag(45,"EUR"),Betrag(-48,"EUR") )
  
    
  val filterNegativeBetraege :List[Betrag] => List[Betrag] = betraege => for( betrag <- betraege; if( betrag < ZERO_EUR ) ) yield betrag  
    
    
  val filterPositiveBetraege = ( betraege :List[Betrag] ) => for( betrag <- betraege; if( betrag > ZERO_EUR ) ) yield betrag 
      
    
  val filterNullBetraege = ( betraege :List[Betrag] ) => for( betrag <- betraege; if( betrag == ZERO_EUR ) ) yield betrag   
         
     
  val filterGeradeBetraege = ( betraege :List[Betrag] ) => for( betrag <- betraege; if( betrag.value % 2 == 0 ) ) yield betrag    
       
     
  val filterUngeradeBetraege = ( betraege :List[Betrag] ) => for( betrag <- betraege; if( betrag.value % 2 != 0 ) ) yield betrag   
}