package alicia.zahlen

object ZahlenPraesentation extends Application {

	val einkaufsListe0 : List[String]   =   Nil
	
	val einkaufsListe1 = "Milch" :: einkaufsListe0
	val einkaufsListe2 = "Eier" :: einkaufsListe1
	val einkaufsListe3 = "Saure Zitronen" :: einkaufsListe2
	
//	println( einkaufsListe3 )
//	println( einkaufsListe1 )
	
	
	val zahlListe : List[Int]  =  4 :: 7 :: 3 :: 2 :: 5 :: Nil
	
	val zahl = 47325
	
//	println( zahl )
//	println( zahlListe )
	


	
	val stellen  =  ( zahl : Int ) => {
		
		var vergleichzahl = 10
		
		var stellen = 1
		
		while( zahl > vergleichzahl ){
		  
			vergleichzahl = vergleichzahl * 10	
			
			stellen = stellen + 1
		}
		
		stellen
	}
	
	
	//println( stellen( 456489898 ) )
	
	
	
	val wieoft = ( teiler :Int, z :Int ) => {
		
		var zahl = z
		
		var wieoft = 0
		
		while( zahl > teiler ){
			
			wieoft = wieoft + 1
			
			zahl = zahl - teiler
		}
		
		wieoft
	}
	
	
	val mal10 = ( stelle :Int ) => {
		
		var zaehler = 1
		
		var ergebnis = 1
		
		while( zaehler < stelle ){
			
			ergebnis = ergebnis * 10
			
			zaehler = zaehler + 1
		}
		
		ergebnis
	}
	
	
	println( mal10( 4 ) )
	
	
	val separiere = ( zahl :Int ) => {
		
		var stelle = stellen( zahl )
		
		var list : List[Int] = Nil
		
		while( stelle > 0 ){
			
			wieoft( mal10( stelle ) , zahl )
			
		}
		
	}
	
	
	
	//println( wieoft( 1000, 735 ) )
	
	
	
	val teile : (Int,Int,Int) => Int
	    = 
		( teiler :Int, zahl :Int, wieoft : Int ) =>  if( zahl > teiler ) wieoft else teile( zahl - teiler, teiler, wieoft + 1 )
	
	
	
	
}