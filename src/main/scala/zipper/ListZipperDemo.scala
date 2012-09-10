package zipper

object ListZipperDemo {


	
	// first element: the current sub list under focus (the 'rest' of the list we currently stepped 'down')
	// second element: the breadcrumps: each element is a single crump, holding the element of the head (parent)
	// from which we stepped down up to the current sub list under focus
	type ListZipper[A] = ( List[A], List[A] )
	
	
	def goForward[A]( zipper :ListZipper[A] ) : ListZipper[A] = zipper match {
		
		case( x :: xs, crumps ) => ( xs,  x :: crumps )
		case _					=> zipper
	}
	
	def goBack[A]( zipper :ListZipper[A] ) : ListZipper[A] = zipper match {
		
		case( xs, x :: crumps ) => ( x :: xs,  crumps )
		case _					=> zipper
	}	
	
	
}