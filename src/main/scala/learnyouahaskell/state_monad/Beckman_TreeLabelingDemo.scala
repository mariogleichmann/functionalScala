package learnyouahaskell.state_monad

import typeclassopedia.monad.Monad._
import typeclassopedia.monad._


object Beckman_TreeLabelingDemo extends Application{

  abstract class Tree[A]
  case class Leaf[A]( a :A ) extends Tree[A]
  case class Branch[A]( left :Tree[A], right :Tree[A] ) extends Tree[A]
  
  
  val tree1 = Branch(
		  			Leaf( "a" )
		  			,
		  			Branch(
		  			     Branch(
		  			        Leaf( "b" )
		  			        ,
		  			        Leaf( "c" )
		  			     )
		  			     ,
		  			     Leaf( "d" )
		  			)
  			  )
  
  			  
  type ST = Int // THE 'STATE' 	  
  	
  type LabeledTree[A] = Tree[(ST,A)]
  
  
  // non-monadic labeling
  def label[A] : Tree[A] => LabeledTree[A] =
    
    tree => {
      
      // inner 'function' (where lab = ... )
      // non-monadicly you need to pass the state explicitly around to all involved functions! (here, the second arg 'st')
      def lab[A] : Tree[A] => ST => (ST, LabeledTree[A] )
      		=
      		tr => st => (tr,st) match {
      		  
      		  case (Leaf(content), n) => ( n+1, Leaf( (n, content) ) )
      		  
      		  case (Branch(l,r), n0)  => {  val (n1,labLeft) = lab(l)(n0)
      			  							val (n2,labRight) = lab(r)(n1)
      			  							
      			  							( n2, Branch(labLeft,labRight) )
      		  							 }
      		} 
      	
      	lab( tree )( 0 )._2
    }
      
  println( " label: " + label( tree1 ) )	
  	
  	  	
  // monadic labeling
  	
  // 1) an instance for the 'Labeled-Monad'
  	
  type Labeled[A] =  ST => (ST,A)

  implicit object LabeledMonad extends Monad[Labeled]{
	
	def Return[A]( content :A ) =  st => ( st, content )
	
	def bind[A,B]( la : Labeled[A] )( f : A => Labeled[B] ) : Labeled[B] =
	  
	  st0 => {  
	    
	    val ( st1, a ) = la( st0 )
		val lb :Labeled[B] = f( a )
	    
		lb( st1 )
	}
  }
  	
  // 2) a Labeled factory
	
  def updateState :Labeled[Int] =   n => ( n+1,n )
  
  def deliver[A] : A => Labeled[A] =   x => ( st => ( st, x )  )
	
  // the monadic label function
  
  def mlabel[A] :Tree[A] => Labeled[LabeledTree[A]] = 
    
    tree => tree match {
      
      case Leaf( x )  	=>  updateState >>= ( n => deliver( Leaf( n,x ) ) )
                            // or alternatively: ( n => Return[Labeled,LabeledTree[A]]( Leaf( n,x ) ) )
      case Branch(l,r)  =>  mlabel( l ) >>= ( labLeft => 
        					mlabel( r ) >>= ( labRight => 
        					  deliver( Branch( labLeft, labRight ) ) ) )
    }

  	
  println( "mlabel: " + mlabel( tree1 )(0)._2 )	
  	 	
  	
}