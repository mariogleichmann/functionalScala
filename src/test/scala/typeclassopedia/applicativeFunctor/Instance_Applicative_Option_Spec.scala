package typeclassopedia.applicativeFunctor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import typeclassopedia.applicativeFunctor.instances.OptionApplicativeFunctor
import typeclassopedia.applicativeFunctor.CommonFunctions._

class OptionApplicativeSpec extends FlatSpec with ShouldMatchers{


	
	
  val add3 = (x:Int) => (y:Int) => (z:Int) => x + y + z

  "Same int values " should " be considered equal" in { 
	  
	 (pure( add3 ):Option[Int=>Int=>Int=>Int]) compose Some(3) compose Some(4) compose Some( 5 ) should equal ( Some(12) )
  }
  
   
	  
  //val cc = (Some(add3):Option[Int=>Int=>Int=>Int]) compose Some(3) compose Some(4) compose Some( 5 )
//  val cc = pure( add3 ) compose Some(3) compose Some(4) compose Some( 5 )
// 
//  println( "cc: " + cc )
//  
//  
//  val pureAdd3 = pure( add3 )
//  
//  val dd = pureAdd3 compose Some( 3 ) compose None compose Some( 5 )
//  
//  println( "dd: " + dd )
//  
//  
	
//	"Same int values " should " be considered equal" in { 
//	  isEquals( 1, 1 ) should equal ( true )
//	}
//	
//	"Different int values " should " not be considered equal" in {	
//	  isEquals( 1, 2 ) should equal ( false )
//	}
	
}