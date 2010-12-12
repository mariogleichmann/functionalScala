package typeclassopedia.yesno

object Instances {

  implicit object IntYesNo extends YesNo[Int]{
    def yesNo( i :Int ) = i match{
      case 0 => false
      case _ => true
    }
  }
  
  implicit object listYesNo extends YesNo[List[_]]{
    def yesNo( l :List[_] ) = l match {
      case Nil => false
      case _   => true
    }
  }
  
  implicit object booleanYesNo extends YesNo[Boolean]{
	  def yesNo( b :Boolean ) = b
  }
  
  implicit object optionYesNo extends YesNo[Option[_]]{
	  def yesNo( opt :Option[_] ) = opt match {
	 	  case None => false
	 	  case Some(_) => true
	  }
  }
}
