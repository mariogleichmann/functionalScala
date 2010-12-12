package typeclassopedia.eq

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import typeclassopedia.eq.instances.StringEq

class StringEqSpec extends FlatSpec with ShouldMatchers with EqApplication {

	"Same string values " should " be considered equal" in {	
	  isEquals( "1", "1" ) should equal ( true )
	}
	
	"Different string values " should " not be considered equal" in {
	  isEquals( "1", "2" ) should equal ( false )
	}
	
}