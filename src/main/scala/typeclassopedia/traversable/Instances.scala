package typeclassopedia.traversable

import typeclassopedia.applicativeFunctor.ApplicativeFunctor
import typeclassopedia.applicativeFunctor.instances.OptionApplicativeFunctor
import typeclassopedia.applicativeFunctor.instances.ListApplicativeFunctorMult
//import typeclassopedia.applicativeFunctor.instances.ListApplicativeFunctor // like zip

object instances {
	
	sealed trait Tree[+A]
	case object Leaf extends Tree[Nothing]
	case class Node[A]( value :A, left :Tree[A], right :Tree[A] ) extends Tree[A]
	
	
	implicit object TreeTraversable extends Traversable[Tree]{
		
		def traverse[AF[_],A,B] ( f : A => AF[B] )( tree :Tree[A] )( implicit apFunctor :ApplicativeFunctor[AF] ) : AF[Tree[B]] = tree match {
			
			case Leaf			 => apFunctor.pure( Leaf )
			
			case Node( v, l, r ) => {	
										val node : B => Tree[B] => Tree[B] => Tree[B] 
										    = 
										    (vlue:B) => (lft :Tree[B]) => (rght :Tree[B]) => Node( vlue, lft, rght )
										    
										val liftedNode :AF[B => Tree[B] => Tree[B] => Tree[B]] = apFunctor.pure( node )
				
										val left :AF[Tree[B]] = traverse(f)(l)
										val right :AF[Tree[B]] = traverse(f)(r)
										val value :AF[B] = f(v)
										println( "value : " + value )
										
	
										val nod :AF[Tree[B]=>Tree[B]=>Tree[B]] = apFunctor.ffmap( liftedNode )( value )
										
										println( "nod : " + nod )
										
										val no :AF[Tree[B]=>Tree[B]] = apFunctor.ffmap( nod )( left )
										
										val treeb :AF[Tree[B]] = apFunctor.ffmap( no )( right )
										
										treeb
									}
		}
		

		
		def sequence[AF[_],A]( tree :Tree[AF[A]] )( implicit apFunctor :ApplicativeFunctor[AF] ) : AF[Tree[A]] = tree match {
			
			case Leaf => apFunctor.pure( Leaf )
			
			case Node( afVal, afLeft, afRight ) => {
														
													val node : A => Tree[A] => Tree[A] => Tree[A] 
													    = 
													   (vlue:A) => (lft :Tree[A]) => (rght :Tree[A]) => Node( vlue, lft, rght )
										    
													val liftedNode :AF[A => Tree[A] => Tree[A] => Tree[A]] = apFunctor.pure( node )		
													
													
													val nod :AF[Tree[A]=>Tree[A]=>Tree[A]] = apFunctor.ffmap( liftedNode )( afVal )
													
													val no :AF[Tree[A]=>Tree[A]] = apFunctor.ffmap( nod )( sequence( afLeft ) )
										
													val treeb :AF[Tree[A]] = apFunctor.ffmap( no )( sequence( afRight ) )
										
													treeb
										}
		}
	
	}

	
	// usage examples ...
	
	val tree1 : Tree[Int] = 
					Node( 1,
							Node( 2, Leaf, Leaf ),
							Node( 3,
									Node( 4, Leaf, Leaf ),
									Node( 5, Leaf, Leaf )
							)
					)
					
	val tree2 : Tree[Int] = 
				Node( 1,
						Node( 2, Leaf, Leaf ),
						Node( 0,
								Node( 4, Leaf, Leaf ),
								Node( 5, Leaf, Leaf )
						)
				)
					
	val toStringOpt : Int => Option[String] = ( x :Int ) => if( x == 0 ) None else if ( x % 2 == 0 ) Some( x + " is Even!" ) else Some( x + " is Odd!" )
					
	println( "toStringOpt tree1 : " + TreeTraversable.traverse( toStringOpt )( tree1 ) )
	
	println( "toStringOpt tree2 : " + TreeTraversable.traverse( toStringOpt )( tree2 ) )
	
	
	
	val toIntList :Int => List[Int] = (x :Int ) => x :: x*10 :: Nil
	
	println( "toIntList tree1 : " + TreeTraversable.traverse( toIntList )( tree1 ) )
	
	
	def main( args :Array[String]) = {
	}
	
}