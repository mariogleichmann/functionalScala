package higherOrderFunctions

object CurryingSamples extends Application{



  // -----------------------------------------------------
  object Sum{
    abstract trait OR[+A,+B]
    case class FST[A,B]( a:A ) extends OR[A,B]
    case class SND[A,B]( b:B ) extends OR[A,B]
  } 
  
  object Product{
    
   import Sum._
    
  // you could compose them (is this right or left associative ??? thus OR[A,OR[B,C]] or OR[OR[A,B],C] ???)
  type xx[A,B,C]  =  Int => A OR B OR C
  
  
  
  type x[A,B]  =  Int => A OR B
  
  // pair extractor
  object x { 
	def unapply[A,B]( p :A x B ) : Option[(A,B)] = Some( ( fst(p),snd(p) ) )
  }
  
  // pair constructor
  def pair[A,B]( a :A, b :B ) : A x B  =   i => if( i == 0 ) FST(a) else SND(b)

  // making 'x' a method on any type
  implicit def toX[A]( a : A ) = new Object{
    def x[B]( b :B ) : A x B = pair( a, b )
  }   

  // selectors
  def fst[A,B]( p :A x B ) :A = p(0) match{ case FST(a) => a }
  def snd[A,B]( p :A x B ) :B = p(1) match{ case SND(b) => b }

  // alternative selector, using deconstructive pattern matching
  def fstAlternative[A,B]( p :A x B ) :A = {
    val FST(a) = p(0)
    a
  } 

   def sndAlternative[A,B]( p :A x B ) :B = {
     val SND(b) = p(1)
     b
  } 

  }

  
  import Product._
  
  type XCoord = Int
  
  type YCoord = Int
  
  type Point = XCoord x YCoord
  
  // extractor
  object Point { 
	def unapply[A,B]( p :Point ) : Option[(XCoord,YCoord)] = Some( ( xCoord(p),yCoord(p) ) )    
  }
  
  type Radius = Int
  
  type Circle = Point x Radius
  
  
  // allg. functions
  //def fst[A] ( p : (A,_) ) = p match { case (a,_) => a } 
  //def snd[B] ( p : (_,B) ) = p match { case (_,b) => b } 
  def id[A]( a :A ) :A = a
  def pow : (Int,Int) => Int = ( base, exp ) =>  if( exp <= 0 ) base else base * pow( base, exp-1 )
  
  // constructor
  //val point : (Int,Int) => Point  =  ( x,y ) => (x,y)
  //val point : Point => Point  =  id[Point]
  val point : ( XCoord, YCoord) => Point = (xc,yc) => xc x yc
  // selectors
  val xCoord : Point => XCoord =  p => fst( p )
  val yCoord : Point => YCoord =  p => snd( p )
  
  val p = point( 1, 2 )
  
  // constructor
  //val circle : ( Point, Radius  ) => Circle  =  ( p :Point, r :Radius ) => ( p, r )
  val circle : Point x Radius => Circle  =  id[Circle]
  
  val c = circle( point( 1, 2 ) x 5 )
  
  // selectors
  val center : Circle => Point  =  c => fst( c )
  val radius : Circle => Radius =  c => snd( c )
  
  val isWithin : ( Circle, Point ) => Boolean
  	  = 
  	 ( circle :Circle, point :Point ) => {
    
  	    val Point(a,b) = center( circle )
  		val Point(x,y) = point
    
        pow( x-a, 2 ) + pow( y-b, 2 ) <=  pow( radius( circle ), 2 )

     }
  	 
  val isWithinCircle : Circle => ( Point => Boolean )
  	=
  	circle => {
  	  
  	  val radSquare = pow( radius( circle ), 2 )
  	  
  	  point => {
  	    
  		  val Point(a,b) = center( circle )
  		  val Point(x,y) = point
       
  		  pow( x-a, 2 ) + pow( y-b, 2 ) <= radSquare   	    
  	  }
  	}
  	
  
  	//type Lst[A] = A x Lst[A]
  	
  	
  	// --------------- 2nd version - with case classes
  	
case class PPoint( val x :Int, val y :Int )

case class CCircle( val center :PPoint, val radius :Int )

val ccenter : CCircle => PPoint 
	=
	circle => {
	  val CCircle( center ,_) = circle
	  center
	}
	
val rradius : CCircle => Int
	=
	circle => {
	  
	  val CCircle( point , radius) = circle
	  radius
	}
	


  val iisWithin : ( CCircle, PPoint ) => Boolean
  	  = 
  	 ( circle, point ) => {
    
  	    val PPoint(a,b) = ccenter( circle )
  		val PPoint(x,y) = point
    
        pow( x-a, 2 ) + pow( y-b, 2 ) <=  pow( rradius( circle ), 2 )

     }


  val iisWithinCircle : CCircle => ( PPoint => Boolean )
  	=
  	circle => {
  	  
  	  val radSquare = pow( rradius( circle ), 2 )
  	  val PPoint(a,b) = ccenter( circle )
  	  
  	  point => {
  		  
  		  val PPoint(x,y) = point
       
  		  pow( x-a, 2 ) + pow( y-b, 2 ) <= radSquare   	    
  	  }
  	}  	
  	
  	
  	val tower = CCircle( PPoint(2,2), 3 )
  	
  	val towerDetects = iisWithinCircle( tower )
  	
  	println( towerDetects( PPoint( 3,4 ) ) )
  	println( towerDetects( PPoint( 2,2 ) ) )
  	println( towerDetects( PPoint( 6,6 ) ) )
  	println( towerDetects( PPoint( 8,2 ) ) )
  	
  	
  	
  	// --- mechanical currying using a curry-function
  	
  	def curry[A,B,C]( f :(A,B) => C ) : A => B => C  =  a => b => f(a,b)
  	
  	val curriedWithin : CCircle => PPoint => Boolean = curry( iisWithin )
  	
  	val tower2 = CCircle( PPoint(2,2), 3 )
  	
  	val towerDetects2 = curriedWithin( tower )
  	
  	println( towerDetects2( PPoint( 3,4 ) ) )
  	println( towerDetects2( PPoint( 2,2 ) ) )
  	println( towerDetects2( PPoint( 6,6 ) ) )
  	println( towerDetects2( PPoint( 8,2 ) ) )  	
  	
  	// --- mechanical currying using Scala's curried function
  	
  	val curriedWithin2 = iisWithin.curried
  	
  	val tower3 = CCircle( PPoint(2,2), 3 )
  	
  	val towerDetects3 = curriedWithin2( tower )
  	
  	println( towerDetects3( PPoint( 3,4 ) ) )
  	println( towerDetects3( PPoint( 2,2 ) ) )
  	println( towerDetects3( PPoint( 6,6 ) ) )
  	println( towerDetects3( PPoint( 8,2 ) ) )   	
  	
  	// --- curried methods
  	
  	// here, we can't get 'in between' the calls of the first and second param, 
  	// so we can't do optimization and have to calculate the square of radius every time
   def iiisWithin ( circle: CCircle)( point: PPoint ) : Boolean = {
    
  	    val PPoint(a,b) = ccenter( circle )
  		val PPoint(x,y) = point
    
        pow( x-a, 2 ) + pow( y-b, 2 ) <=  pow( rradius( circle ), 2 )

     } 	
  	
  	// here, we just deliver a function as a result (having the method only taking the first parameter)
  	// so here we can again do optimization, since we get between the call of the first argument and the second
  	def iiisWithinCircle ( circle :CCircle ) : ( PPoint => Boolean ) = {
  	  
  	  val radSquare = pow( rradius( circle ), 2 )
  	  val PPoint(a,b) = ccenter( circle )
  	  
  	  point => {
  		  
  		  val PPoint(x,y) = point
       
  		  pow( x-a, 2 ) + pow( y-b, 2 ) <= radSquare   	    
  	  }
  	}
  	

  // here, we have the completely uncurried version of iswithin as a method	
  	
  def iiisWithine ( circle: CCircle, point: PPoint ) : Boolean = {
    
  	    val PPoint(a,b) = ccenter( circle )
  		val PPoint(x,y) = point
    
        pow( x-a, 2 ) + pow( y-b, 2 ) <=  pow( rradius( circle ), 2 )

   }
  
  // and here, eta expansion kicks in, resulting in the curried version of that method as a function  
    val curriedWithine = curry( iiisWithine )
  	
  	val tower4 = CCircle( PPoint(2,2), 3 )
  	
  	val towerDetects4 = curriedWithine( tower )
  	
  	println( towerDetects4( PPoint( 3,4 ) ) )
  	println( towerDetects4( PPoint( 2,2 ) ) )
  	println( towerDetects4( PPoint( 6,6 ) ) )
  	println( towerDetects4( PPoint( 8,2 ) ) )
}