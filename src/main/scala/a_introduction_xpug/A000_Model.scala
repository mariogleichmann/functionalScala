package a_introduction_xpug

object A000_Model {

  case class Betrag( val value :Int, currency :String ){    
    def +( other :Betrag ) = new Betrag( this.value + other.value, currency )    
    def -( other :Betrag ) = new Betrag( this.value - other.value, currency )   
    def unary_- = new Betrag( -this.value, currency )
    def ==( other :Betrag ) =  this.value == other.value       
    override def toString =  value + " " + currency
    def <( other :Betrag ) = this.value < other.value
    def >( other :Betrag ) = this.value > other.value
  }
  
  val ZERO_EUR = Betrag( 0, "EUR" )
  
  

  
  
  
  
  abstract case class Auftrag( val betrag :Betrag )
  case class KaufAuftrag( b :Betrag ) extends Auftrag( b )
  case class VerkaufAuftrag( b :Betrag ) extends Auftrag( b )
}