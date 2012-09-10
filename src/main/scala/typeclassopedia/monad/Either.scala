package typeclassopedia.monad

object Either {

  trait Either[+E,R]
  case class Left[+E,R]( e :E ) extends Either[E,R]
  case class Right[+E,R]( r :R ) extends Either[E,R] 
}