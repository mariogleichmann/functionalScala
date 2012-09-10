package alicia.ratespiel

object Ratespiel extends Application{

  
  val ratemal = () => {
    
    val zufallszahl = (Math.random * 100).intValue
    
    var zaehler = 0;
    
    Console.print( "Rate eine Zahl: " )
    var ratezahl = Console.readInt
    
    zaehler = 1
    
    
    while( ratezahl != zufallszahl ){
      
      if( zufallszahl > ratezahl ) Console.println( "Meine Zahl ist groesser" )
      
      if( zufallszahl < ratezahl ) Console.println( "Meine Zahl ist kleiner" )
      
      Console.print( "Rate eine weitere Zahl: " )
      ratezahl = Console.readInt
      
      zaehler += 1
    }
    
    Console.println( "Hurra - Du hast die Zahl gefunden !!!" )
    Console.println( "Dafuer hast Du " + zaehler + " Versuche gebraucht" )
  }
  
  
  ratemal()
}