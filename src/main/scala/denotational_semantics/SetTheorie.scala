package denotational_semantics

import typeclassopedia.eq._
import typeclassopedia.eq.eqUtils._
import typeclassopedia.eq.instances.StringEq

object SetTheorie extends Application {

  // the 'essence' of a Set can be reduced to the question >> is 'a' an element of a given Set ? <<
  // so we define a generic MySet-Type as a function that accepts an arbitrary object of type A for which 
  // it than answers 'true' if the given object is element of the Set or 'false' otherwise
  type MySet[A]  =   A => Boolean
  
  // here's the 'constructor' function : it takes an initial object of a type A and returns an instance of MySet[A]
  // (that is a function which can answer that the given object is an element of this MySet-instance. So we return a 
  // ad hoc created function which accepts an arbitrary object of that type A and only returns 'true' if that object
  // equals the initial given object - which is a free variable within that function, so here the function is also a Closure!)
  // Note, that the initial given object needs to by of a type which in turn can be compared to another object of the same
  // type for answering the quastion if the two instances are equal. For that, we require that such a type needs to be an
  // instance of the typeclass Eq !!!
  def unit[A:Eq]( a: A ) :MySet[A]   =   c =>  c equ a 
  //def unit[A:Eq] :  A => MySet[A]     =   a => c => implicitly[Eq[A]].eq( c )( a )
  
  val emptySet :MySet[Nothing] = a => false

  //def isEmpty[A]( set :MySet[A] ) = 
  
  // ... now if we want to know if an arbitrary object (of the Set's type parametrized type A) is within a given Set,
  // we only need to call the Function (which represents the Set) with that given object
  def contains[A] : A => MySet[A] => Boolean   =   a => set => set( a )
  
  // here's another 'constructor' function, which can be used to construct a new list 'on top' of a given list and another
  // object of appropriate type - the result is again a new Set-Instance, therefore again an ad hoc created function, which again
  // accepts an arbitrary object of that type A. If that object is already an element of that given Set-instance, we simply return 
  // that Set-instance without any other 'action'. Otherwise, we again return an ad hoc created function which accepts an arbitrary 
  // object of that type A. Now we first ask, if that object is an element of the initial given Set-Instance (again by closing over 
  // to the given argument of the function) - if so, we're done. If not, the object might be the object we just added - in that case
  // we need to compare that arbitrary given object to that added object (again by closing over) by again relying on the needed
  // typeclass Eq (see function 'unit' above)
  def add[A:Eq]( a :A )( set :MySet[A] ) : MySet[A] =  
    
	  if( contains(a)(set) ) set 
	  else 
	    c => contains( c )( set ) || contains( c )( unit(a) )
	    
//  def add[A:Eq] : A => MySet[A] => MySet[A] =  
//    
//    a => set =>   if( contains(a)(set) ) set 
//    			  else 
//    			    c => set( c ) match {
//    
//    					case true => true
//    					case false => implicitly[Eq[A]].eq( c )( a )
//    			    }
   
  def remove[A:Eq]( a :A )( set :MySet[A] ) : MySet[A] = 
    
    if( !contains(a)(set) ) set
    else
      c => contains(c)(set) && !contains(c)(unit(a))
	    
	    
//  def map[A,B]( set :MySet[A] )( f : A => B ) :MySet[B] =
    

      
      

  
  val aSet :MySet[String] = unit( "a" )
  
  println( "contains a aSet : " + contains("a")( aSet ) )
  println( "contains z aSet : " + contains("z")( aSet ) )
  
  val abSet = add( "b" )( aSet )
  
  println( "contains a abSet : " + contains("a")( abSet ) )
  println( "contains b abSet : " + contains("b")( abSet ) )
  println( "contains z abSet : " + contains("z")( abSet ) )  
  
  val abcSet = add( "c" )( abSet )
  val acSet = remove( "b" )( abcSet )
  
  println( "contains a abcSet : " + contains("a")( abcSet ) )
  println( "contains b abcSet : " + contains("b")( abcSet ) )
  println( "contains c abcSet : " + contains("c")( abcSet ) )
  println( "contains z abcSet : " + contains("z")( abcSet ) ) 
  
  println( "contains a acSet : " + contains("a")( acSet ) )
  println( "contains b acSet : " + contains("b")( acSet ) )
  println( "contains c acSet : " + contains("c")( acSet ) )
  println( "contains z acSet : " + contains("z")( acSet ) ) 
    
}