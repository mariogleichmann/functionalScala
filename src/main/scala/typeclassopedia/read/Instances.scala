package typeclassopedia.read

object Instances {

	implicit object IntRead extends Read[Int]{
		
		override def read( s :String ) : Int = Integer.valueOf( s ).intValue
	}
	
	implicit object BooleanRead extends Read[Boolean]{
		
		override def read( s :String ) : Boolean = s match {
			case "true" | "True" => true
			case _               => false
		}
	}
}