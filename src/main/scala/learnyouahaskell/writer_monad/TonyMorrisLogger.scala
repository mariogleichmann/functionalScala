package learnyouahaskell.writer_monad

object TonyMorrisLogger extends Application{

	// monoid typeclass
	trait Monoid[A] {
		def append(a1: A, a2: A): A
		def empty: A
	}
 
	// monoid instances
	object Monoid {
		
		// List[A] is an instance of monoid
		implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
			def append(a1: List[A], a2: List[A]) = a1 ::: a2 // list concatenation
			def empty = Nil // empty list
		}
		
		// function of type A=>A is an instance of monoid
		implicit def FuncMonoid[A]: Monoid[A => A] = new Monoid[A => A]{
			def append(f :A=>A, g :A=>A ) = (x :A) => f( g( x ) ) // function composition
			def empty = ( x :A ) => x // identity function
		}
	}
 
	// this is our Logger monad (not an implementation of a typeclass, but a direct implementation!)
	case class Logger[LOG, A](log: LOG, value: A) {
		
		def map[B](f: A => B) = Logger( log, f(value) ) // functor: map over the value, log remains unchanged
 
		def flatMap[B](f: A => Logger[LOG, B])(implicit m: Monoid[LOG]) = { // fmap over value - append new log to old log to receive new log 
			val x = f( value )
			Logger( m.append(log, x.log), x.value )
		}
	}
 
	object Logger {
		def unital[LOG, A](value: A)(implicit m: Monoid[LOG]) = Logger( m.empty, value ) // like typeclass monad function: return
	}
 

	// util functions for easy Logger creation while using List[String] as log 
	object Util {
		// utility
		implicit def ListLogUtil[A](a: A) = new {
			
			def ~>[B](b: B) = Logger( List(a), b )
 
			def <|~[B](k: A => B) = Logger( List( k(a) ), a )
		}
 
		def noLog[A](a: A) = Logger.unital[List[String], A](a)
		
	}
	
	// util functions for easy Logger creation while using A => A as log
	object FuncUtil {
		
		// utility
		implicit def FuncLogUtil[A](f: A=>A) = new {
			
			def **>[B](b: B) = Logger( f, b )
 
		}
		def noLogss[B,A](a: A) = Logger.unital[B=>B, A](a)
		
		
		//def noLogss[B] = (a :A) => Logger( ( x :B ) => x, a )
	}
	
	
	
  //import Util._
	import FuncUtil._
 
    val x = 4 // parse int from command line
    
//    val r = 
//      for(a <- ao;
//          b <- intString(a);
//          c <- lengthIsEven(b);
//          d <- noLog(hundredOrThousand(c));
//          e <- times7(d)
//         ) yield e
// 
//    println("RESULT: " + r.value)
//    println
//    println("LOG")
//    println("---")
    //r.log foreach println

 
//  def addOne(n: Int) : Logger[List[String],Int] = 
//    ("adding one to " + n) ~> (n + 1)
// 
//  def intString(n: Int) : Logger[List[String],String] = 
//    ("converting int to string " + n) ~> n.toString
// 
//  def lengthIsEven(s: String) : Logger[List[String],Boolean] =
//    ("checking length of " + s + " for evenness") ~> (s.length % 2 == 0)
// 
//  def hundredOrThousand(b: Boolean) : Int = // no logging
//    if(b) 100 else 1000
// 
//  def times7(n: Int) : Logger[List[String],Int] =
//    (n * 7) <|~ ("multiplying " + n + " by 7 to produce " + _)
  
    
    
    // for comprehension works with arbitrary objects which are providing a map and flatMap function (see our monad class Logger)
    val res =
    	for{ a <- addOne(x) 
    	 	 b <- intString(a) 
    	 	 c <- lengthIsEven(b)
    	 	 d <- noLogss[Double,Int]( hundredOrThousand(c) )
    	 	 e <- times7(d)
    	} yield e
 
    println( "amount : " + res.log( 0 ) )
    
    def addOne(n: Int) : Logger[Double=>Double,Int] = {
    
	  val s :Double = System.currentTimeMillis
	  
	  val result:Int = (n + 1)
	  
	  val e :Double = System.currentTimeMillis + 1000
	  
	  ( (x:Double) => (e - s) + x  ) **> result
   }
 
  def intString(n: Int) : Logger[Double=>Double,String] = {
	  
	  val s :Double = System.currentTimeMillis
	  
	  val result = n.toString
	  
	  val e :Double = System.currentTimeMillis + 1000	  
     
	   ( (x:Double) => (e - s) + x  ) **> result
 
  }
    
  def lengthIsEven(str: String) : Logger[Double=>Double,Boolean] = {
	  
	  val s :Double = System.currentTimeMillis
	  
	  val result = (str.length % 2 == 0)
	  
	  val e :Double = System.currentTimeMillis + 1000	  
     
	   ( (x:Double) => (e - s) + x  )**> result	  
  }

 
  def hundredOrThousand(b: Boolean) : Int = // no logging
    if(b) 100 else 1000
 
  def times7(n: Int) : Logger[Double=>Double,Int] = {
    	
   	  val s :Double = System.currentTimeMillis
	  
	  val result = (n * 7)
	  
	  val e :Double = System.currentTimeMillis + 1000	
	  
	   ( (x:Double) => (e - s) + x  ) **> result	 
    	
  }
    
  


}