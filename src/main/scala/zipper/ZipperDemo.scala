package zipper

object ZipperDemo {

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
		
	trait Direction
	case object Left extends Direction
	case object Right extends Direction
	
	type Directions = List[Direction]
	
	
	 // reconstructs a new tree from the given tree argument, where the Node at the position specified by the 
	 // given path of directions argument (starting from the root) will be replaced by a new Node which holds
	 // the new passed value (see argument newValue) instead of the original Nodes old value and still/again
	 // refers to the original Nodes child trees
	 // this way, we 'changed' the value of a given Node 
	 def change[A]( newValue :A )( dirs :Directions )( tree :Tree[A] ) : Tree[A] = (dirs, tree ) match {
	  	
	 	  case ( Left :: ds,  Node( x, l, r ) ) => Node( x, change( newValue )( ds )( l ), r )
	 	  case ( Right :: ds, Node( x, l, r ) ) => Node( x, l, change( newValue )( ds )( r ) )
	 	  case ( Nil, 		  Node( _, l, r ) ) => Node( newValue, l, r )
	  }
	   
	 // delivers the value for a Node within the given tree, specified by the given path of directions  
	 def elementAt[A]( dirs :Directions )( tree :Tree[A] ) : A = ( dirs, tree ) match {
		 
	 	case ( Left :: ds,  Node( _, l, _ ) ) => elementAt( ds )( l )
	 	case ( Right :: ds, Node( _, _, r ) ) => elementAt( ds )( r )
	 	case ( Nil, 		Node( x, _, _ ) ) => x		 
	 }
	 
	 
	 type Breadcrumps = List[Direction]
	 
	 def goLeft[A]( t_b :(Tree[A],Breadcrumps) ) : ( Tree[A], Breadcrumps ) = {
		 
		 val( tree, breadcrumps ) = t_b
		 
		 tree match {
	
		 	case Node( _, l, _ ) => ( l, Left :: breadcrumps )
		 	case Empty           => ( tree, breadcrumps )
		 }
	 }
	 
	 
	 def goRight[A]( t_b :(Tree[A],Breadcrumps) ) : ( Tree[A], Breadcrumps ) = {
		 
		 val( tree, breadcrumps ) = t_b
		 
		 tree match {
	
		 	case Node( _, _, r ) => ( r, Right :: breadcrumps )
		 	case Empty           => ( tree, breadcrumps )
		 }
	 } 
	 
	 // now we can write goLeft and goRight as Functions instead of methods
	 trait TreeFuncs[A]{
		 		 
		 val goLeft : ( Tuple2[Tree[A],Breadcrumps] ) => ( Tree[A], Breadcrumps ) 
		    = 
		    ( t_b :(Tree[A],Breadcrumps) )=>{
			 
			 val( tree, breadcrumps ) = t_b
			 
			 tree match {
		
			 	case Node( _, l, _ ) => ( l, Left :: breadcrumps )
			 	case Empty           => ( tree, breadcrumps )
			 }
		 }
		 
		 
		 val goRight : ( Tuple2[Tree[A],Breadcrumps] ) => ( Tree[A], Breadcrumps ) 
		    = 
		    ( t_b :(Tree[A],Breadcrumps) )=> {	 
			 val( tree, breadcrumps ) = t_b
			 
			 tree match {
		
			 	case Node( _, _, r ) => ( r, Right :: breadcrumps )
			 	case Empty           => ( tree, breadcrumps )
			 }
		 } 		 
	 }
	
	   
	   
	def main( args :Array[String] ) = {
		
		println( freeTree )
		
		println( elementAt( Right :: Left :: Nil )( freeTree ) )
		
		println( change( "P" )( Right :: Left :: Nil )( freeTree ) )
		
		println( goLeft( goRight( freeTree, Nil ) ) )
		
		// here we use -< to switch the order between argument and function
		// but therefore, we need real functions, which are defined within trait TreeFuncs

		// since we are working with a Tree[String], we need our tree-function - instances 
		// type parametrized with String
		val tree = new TreeFuncs[String]{}
		
		// here's the definition of -< for exchanging the positions of function and its argument
		implicit def toFunctionReceiver[A]( arg :A ) = new Object{
			def :-[B]( func :A => B ) : B = func( arg ) 
		}
		
		// ... and here it is: it's now clearer to read that we first go right then left ...
		println(  ( freeTree, Nil :Breadcrumps ) :- tree.goRight :- tree.goLeft  )
	}
	   
	
}