package streams

object RandomMathTests{

  val MAX_NUMBER_VALUE = 100
  val MAX_RESULT = 100
  val MIN_RESULT = 0
  val NUMBER_OF_EXERCISES = 100
  val SYMBOL_ADD = "+"
  val SYMBOL_SUB = "-"
	  
  type OpSymbol = String
  type Operation = (Int,Int) => Int
  type NumPair = (Int,Int)
	
  def randomNum :Stream[Int] = Stream continually ( util.Random.nextInt( MAX_NUMBER_VALUE - 1 ) + 1 )
  val randomNumPairs :Stream[NumPair] = randomNum zip randomNum
  
  val add : (OpSymbol,Operation) = ( SYMBOL_ADD, _ + _ )
  val sub : (OpSymbol,Operation) = ( SYMBOL_SUB, _ - _ )
  val randomOp :Stream[(OpSymbol,Operation)] = ( Stream continually util.Random.nextInt( 2 ) ) map { case 0 => add; case 1 => sub }
  
  val tests :Stream[( (OpSymbol,Operation),NumPair )] = randomOp zip randomNumPairs
  
  val validTests : Stream[( (OpSymbol,Operation),NumPair )]
    =  
    for{ validTest @ ( ( _ , op ), (num1, num2) ) <- tests
		   val result = op( num1, num2 )
		   if( result >= MIN_RESULT && result <= MAX_RESULT )
  		}
  		yield validTest

 
  def main( args :Array[String] ){
  			
    var output = ""
    var i = 0
  
    for( ( (opSymbol, _ ), (num1, num2) ) <- validTests take NUMBER_OF_EXERCISES ){
	  val testSnippet = num1 + " " + opSymbol + " " + num2
	  output += testSnippet + " " * ( 8 - testSnippet.length ) + " = ______ " + " " * 10
	  i += 1
	  if( i % 3 == 0 ) output += "\n"
    }
  
    println( output )  			
  }  		
  		
}