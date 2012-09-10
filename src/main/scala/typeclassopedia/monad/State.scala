package typeclassopedia.monad

import typeclassopedia.monad.Monad._

object State extends Application {

  // THIS IS the State Monad (the 'container' aka 'computational context'
  case class State[+A, S]( f: S => (A,S) ) {
	def runState : S => (A,S)   =   f
  }
  
  // some utility-functions
  
  def get[S] = State( ( s :S ) =>  ( s, s ) ) 
  
  def put[S]( s :S ) = State( ( _ :S ) => ( (), s ))

  
  
  // sample - a Stack-State
 
  type Stack[A] = List[A]
 
  def push[A]( x :A ) : State[Option[A],Stack[A]] = State( ( stack : Stack[A] ) => ( None, x :: stack ) )
  
  def maybePush[A]( x :Option[A] ) : State[Option[A],Stack[A]] = 
	  
	  State( ( stack : Stack[A] ) => ( None, x match { 
		                  					     case Some(i) => i :: stack 
		                  					     case _       => stack } ) )
  
 
  def pop[A]() = State( ( stack : Stack[A] ) => stack match {
	 
	 case x :: xs => ( Some(x), xs )
	 case _       => ( None, stack )
  } )
 
  
  import typeclassopedia.applicativeFunctor.instances._
  
  val mult = (x:Int) => (y:Int) =>  x * y	  
  
  val resultp = for {
      _ <- push(3)
      _ <- push(5)
      _ <- push(7)
      _ <- push(9)
      x <- pop[Int]
      y <- pop[Int]
      _ <- maybePush( mult <%> x <*> y )
      r <- get

    } yield r

  println( resultp.runState( List( 1 ) ) )
    
    
  val altternativeR =
    push( 3 ) >> 
    push( 5 ) >>
    push( 7 ) >>
    push( 9 ) >>
    pop[Int]  >>= ( x => 
    pop[Int]  >>= ( y => 
    maybePush( mult <%> x <*> y ) >>
    get
    ) )    
 
  println( altternativeR.runState( List( 1 ) ) )
  
}