package typeclassopedia.eq

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import typeclassopedia.eq.instances.IntEq

class IntEqSpec extends FlatSpec with ShouldMatchers with EqApplication{
		
	"Same int values " should " be considered equal" in { 
	  isEquals( 1, 1 ) should equal ( true )
	}
	
	"Different int values " should " not be considered equal" in {	
	  isEquals( 1, 2 ) should equal ( false )
	}
	
}