package typeclassopedia.eq

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import typeclassopedia.eq.instances.pairEq
import typeclassopedia.eq.instances.IntEq
import typeclassopedia.eq.instances.StringEq

class PairEqSpec extends FlatSpec with ShouldMatchers with EqApplication{
		
	"Same pair values " should " be considered equal" in { 
	  isEquals( (1,"1"), (1,"1") ) should equal ( true )
	}
	
	"Different pair values " should " not be considered equal" in {	
	  isEquals( (1,"1"), (2,"2") ) should equal ( false )
	}
	
}