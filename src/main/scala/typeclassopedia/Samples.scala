package typeclassopedia

object Samples {
  
	def main(args : Array[String]) : Unit = {}
	
	{
		import typeclassopedia.eq._
		import typeclassopedia.eq.eqUtils._
		import typeclassopedia.show._
		import typeclassopedia.show.showUtils._
	
		// here's a method / function which forces type A to be an instance of more than only one typeclass
		// (in this case A has to be an instance / member of typeclass Eq AND of typeclass Show)
		def compareAndShow[ A :Eq :Show ]( a1 :A, a2 :A ) {
		
			// using the implicit conversion to 'RichShow' (see typeclassopedia.show.showUtils.toRichShow )
			println( "comparing " + a1.show + " with " + a2.show )
		
			//... or using the convenient show-method (which hides the capturing of the implicit Show-Parameter for us)
			println( "comparing " + show(a1) + " with " + show(a2) )
		
			println( "equals : " +  a1.equals( a2 ) )
		}
	
		// Int is an instance / member of typeclass Eq as well as an instance / member of typeclass Show ...
		import typeclassopedia.eq.instances._	
		import typeclassopedia.show.Instances._
	
		compareAndShow( 2, 4 )
		compareAndShow( 7, 7 )
	}
	
	
	
	{
		import typeclassopedia.read._
		import typeclassopedia.read.readUtils._
		
		def parse[A :Read]( s :String ) : A = read( s )
		
		import typeclassopedia.read.Instances._
		
		// although we gave the compiler a hint by annotating variable i with an appropriate 'target'-Type to which type
		// we'd like to convert the given String, the compiler complains about ambiguous implicit values (because there's
		// more than one typeclass instance / implementation within typeclassopedia.read.Instances._
		//val i :Int = parse( "1" )
		
		// ... so we need to call the Method with the desired target-type already mentioned
		val i = parse[Int]( "1" )
		
		println( "parsed: " + i )
		
		
		val b = parse[Boolean]( "true" )
		

	}
	
}
