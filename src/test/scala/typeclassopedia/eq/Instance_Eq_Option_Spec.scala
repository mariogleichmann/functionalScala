package typeclassopedia.eq

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import typeclassopedia.eq.instances.optionEq
import typeclassopedia.eq.instances.pairEq
import typeclassopedia.eq.instances.IntEq
import typeclassopedia.eq.instances.StringEq

class OptionEqSpec extends FlatSpec with ShouldMatchers with EqApplication{
		
	"Same option values " should " be considered equal" in { 
	  isEquals( Some( 1 ), Some( 1 ) ) should equal ( true )
	}
	
	"Different option values " should " not be considered equal" in {			
	  isEquals( Some((1,"1")) , Some((2,"2")) ) should equal ( false )
	}
	
	"Arbitrary Some and None " should " not be considered equal" in {	
	  isEquals( Some((1,"1")) , None ) should equal ( false )
	}
	
	"None and arbitrary Some " should " not be considered equal" in {	
	  isEquals( None, Some((1,"1")) ) should equal ( false )
	}
	
	"Two Nones " should " be considered equal" in {	
	  isEquals( None : Option[Int] , None ) should equal ( true )
	}
}