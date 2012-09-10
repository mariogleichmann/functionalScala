package zipper

// A TREE ZIPPER

object ZipperStep2Demo {

	trait Tree[+A]
	case object Empty extends Tree[Nothing]
	case class Node[A]( value :A, left :Tree[A], right :Tree[A] ) extends Tree[A]
	
	
	val freeTree : Tree[String] =
		Node( "P",
			Node( "O", 
				Node( "L", 
					Node( "N", Empty, Empty ), 						
					Node( "T", Empty, Empty )
				), 					
				Node( "Y", 
					Node( "S", Empty, Empty ), 	
					Node( "A", Empty, Empty )
				) 
			),				
			Node( "L", 
				Node( "W", 
					Node( "C", Empty, Empty ), 						
					Node( "R", Empty, Empty ) 
				), 					
				Node( "A", 
					Node( "A", Empty, Empty ), 						
					Node( "C", Empty, Empty ) 
				) 
			)
		)
	
	// in a single (bread)crump, we not only remember the direction to which subtree we're stepping down
	// (left child sub-tree -> LeftCrump, right child subtree -> RightCrump)
	// but also the content (value) of the node we're starting from (the parent node of the subtree we're stepping down)
	// and the other sibling child subtree (we're NOT stepping down)
	// with this information, we're able to reconstruct the whole tree, when stepping up from a chosen sub tree
	trait Crump[A]
	case class RightCrump[A]( parentVal :A, leftSibling :Tree[A] ) extends Crump[A]
	case class LeftCrump[A]( parent :A, rightSibling :Tree[A] ) extends Crump[A]
	
	type Breadcrumps[A] = List[Crump[A]]
	
	// a zipper is a data structure which is in fact nothing more than
	// 1. the current sub tree we're stepped down to (the sub or sub sub or sub sub sub ... tree we're actually focussing / under focus)
	// 2. a list of (bread)crumps with which we could reconstruct the whole tree when stepping up again
	type Zipper[A] = ( Tree[A], Breadcrumps[A] )
	
	
	// stepping down to the left sub tree
	def goLeft[A]( zipper :Zipper[A] ) : Zipper[A] = {
		 
		 val( tree, breadcrumps ) = zipper
		 
		 tree match {
	
		 	case Node( value, left, right ) => ( left, LeftCrump( value, right ) :: breadcrumps )
		 	case Empty           			=> ( tree, breadcrumps )
		 }
	 }
	 
	 // stepping doen to the right sub tree
	 def goRight[A]( zipper :Zipper[A] ) : Zipper[A] = zipper match {
		 
		 case ( Node( value, left, right ), breadcrumps ) => ( right, RightCrump( value, left ) :: breadcrumps )
		 case ( Empty, _ )           					  => zipper
	 } 
	 
	// stepping up
	def goUp[A](  zipper :Zipper[A] )  : Zipper[A] = zipper match {

		 case ( tree, LeftCrump( parentVal, rightSibling ) :: bs ) => ( Node( parentVal, tree, rightSibling ), bs )
		 case ( tree, RightCrump( parentVal, leftSibling ) :: bs ) => ( Node( parentVal, leftSibling, tree ), bs )
		 case _												    => zipper
	 } 
	 
	// 'modifying' the root of the current sub tree (in focus) 
	// => replacing the current node with another node which features a new value (calculated by the given function 'modifier')
	def modify[A]( modifier : A => A, zipper :Zipper[A] ) : Zipper[A] = zipper match {
		
		case ( Node( value, l, r ), crumps ) => ( Node( modifier( value ), l, r ), crumps )
		case ( Empty, _ )					 => zipper
	}
	
	// replaces the current sub tree (in focus) with the passed tree (this function's sometimes called 'attach')
	// (if the current subtree under focus is a leaf (an Empty tree), than the passed tree is attached to the given tree)
	def replace[A]( tree :Tree[A], zipper :Zipper[A] ) : Zipper[A] = zipper match {
		
		case ( _, crumps ) => ( tree, crumps )
	}
	
	// traverses back / steps up until the top most root of the tree
	def topMost[A]( zipper :Zipper[A] ) : Zipper[A] = zipper match {
		
		case( tree, Nil ) => zipper
		case _  		  => topMost( goUp( zipper ) )
	}
}