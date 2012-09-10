package abstract_types

import scala.collection.mutable.Set

object ParametricTypeInstantiation extends Application{

	
  class Test[T](implicit factory : () => T) {
    val testVal :T = factory()
    var testVar :T = _
    def setVar( x :T ) = testVar = x
  }
  
  object Factories {
	// for picking an appropriate implicit method, the compiler looks at the result type of each method!!!
    implicit def listfact[X]() = List[X]()  // ... so this method has result type 'List[X]' for whatever type X it is called - type inferenced
    implicit def setfact[X]() = scala.collection.mutable.Set[X]()
    implicit def Intfact[X]() : Int = 0 // ... here, we explicitly noted the result type of the method
    // etc
  }


  import Factories._
  val ts = new Test[Set[String]]
  println( ts.testVal ) 
  ts.testVal.add( "a" )
  // ts.testVal.add( 1 )  // error: type mismatch: found: Int(1) required: String
  println( ts.testVal )
  // ts.setVar( Set( 1 ) ) // error: type mismatch: found: Int(1) required: String
  ts.setVar( Set( "b" ) )
  println( ts.testVar )
  
  val tli = new Test[List[Int]]
  println( tli.testVal )
  
  val ti = new Test[Int]
  println( ti.testVal )
  ti.setVar( 2 )

}