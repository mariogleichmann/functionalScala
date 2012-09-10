package algebraic_datatypes

object HeterogenousList extends Application{

  sealed trait HList

  final class HNil extends HList {
    def ::[T](v : T) = HCons(v, this)
  }

  val HNilObj = new HNil()

  final case class HCons[H, HLST <: HList](head : H, tail : HLST) extends HList {
    def ::[T](v : T) = HCons(v, this)
  }

  type :-:[H, HLST <: HList] = HCons[H, HLST]
  
  
  val list : Int :-: String :-: Boolean :-: HNil = 10 :: "hello" :: true :: HNilObj
  
  val list2 : String :-: Boolean :-: HNil = ( "hello" :: ( true :: HNilObj ) )
  val list3 : Int :-: (String :-: Boolean :-: HNil) = 10 :: list2
  
}