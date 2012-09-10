package streams

object RandomNumRows extends Application{
	
	val NUM_ROWS = 10
	val NUM_COLS = 10
	val MAX_VALUE = 100
	
	//var rows : List[List[Int]] = Nil
	
	for( _ <- 1 to NUM_ROWS ){
		
		var row = ""
		var delim = ""
		
		for( _ <- 1 to NUM_COLS ){
			
			row = row + delim + util.Random.nextInt( MAX_VALUE )
			delim = "  ,  "
		}
		
		println( row )
	}
	
	
	

}