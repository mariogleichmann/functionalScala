package typeclassopedia.bifunctor

trait Bifunctor[F[+_, +_]] {
	
  def bimap[A,B,C,D]( fa: F[A,B], f: A => C, g: B => D ) : F[C,D]
}
 
object Bifunctor {
  
  implicit def Tuple2Bifunctor = new Bifunctor[Tuple2] {
	  
    def bimap[A,B,C,D]( fa: Tuple2[A,B], f: A => C, g: B => D ) =  ( f( fa._1 ), g( fa._2 ) )
  }
 
  
  implicit def EitherBifunctor = new Bifunctor[Either] {
	  
    def bimap[A,B,C,D]( fa: Either[A,B], f: A => C, g: B => D ) = 
      fa match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))
      }
  }
}

// ------------------------

trait BifunctorW[F[+_, +_], A, B] {
	
  val value: F[A, B]
  val bifunctor: Bifunctor[F]
 
  def <-:->[C, D](f: A => C, g: B => D) = bifunctor.bimap(value, f, g)
 
  def <-:[C](f: A => C) = bifunctor.bimap(value, f, identity[B])
 
  def :->[D](g: B => D) = bifunctor.bimap(value, identity[A], g)
}

// Wrapping an instance of an arbitrary member of the Bifunctor typeclass 
// (e.g. an Either-Instance, for Either being an instance of typeclass Bifunctor 
//  (that is, there is an implementaion for Bifunctor[Either], makint Either a member of that typeclass ) )
object BifunctorWrapper {
	
  // a somewhat verbose but handy trick for partially applying type variables
  // here, we're firstly only interested in a type constructor with higher kind [F[+_, +_]
  // (could also be a container, which isnt a member of typeclass Bifunctor)
  def bifunctor[F[+_, +_]] :  BifunctorApply[F] = new BifunctorApply[F] {
    
	  // but  here, the type constructor needs an implementation for typeclass Bifunctor (see implicit argument)
	  def apply[A, B](v: F[A, B])( implicit b: Bifunctor[F] ) : BifunctorW[F, A, B] = 
        new BifunctorW[F, A, B] {
          val value = v
          val bifunctor = b
        }
  }
 
  trait BifunctorApply[F[+_, +_]] {
    def apply[A, B](v: F[A, B])(implicit b: Bifunctor[F]): BifunctorW[F, A, B]
  }
 
  import Bifunctor._
 
  
//  implicit def Tuple2Bifunctor[A,B](v: (A, B)) = bifunctor[Tuple2](v)
 
  implicit def Either2Bifunctor[A,B](v: Either[A, B]) = 
    bifunctor[Either](v)
}


object Main {
  def main(args: Array[String]) {
    import BifunctorWrapper._
 
    val x: Either[Int, String] = Left(7)
    val y = ("hello", 42)
 
    val f = (n: Int) => n + 1
    val g = (s: String) => s.reverse
    val h = (s: String) => s.toUpperCase
    val i = (n: Int) => n * 2
 
   // Pretty neat eh?
    val p = f <-: x :-> g
    //val q = h <-: y :-> i
 
    // $ scala Main
    // Left(8)
    // (HELLO,84)
    println(p)
    //println(q)
  }
}