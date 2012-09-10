package streams

import  algebraic_datatypes.List_n_Trees._

object TicTacToe {

	// a special 'Stream'-Tree
	trait STree[+A]
	case class SNode[A]( value :A, childs : Stream[STree[A]] ) extends STree[A]
	


	
	val streamtree =
			SNode( 1, 
				SNode( 2,  Stream.empty ) #:: 
				SNode( 3,  
					SNode( 4,  Stream.empty ) #::
					Stream.empty
				) #:: 
				Stream.empty	
			)
			
	
	
	// reptree f a = Node a (map (reptree f ) (f a))
			
	// endless recursion, because of its strict nature (in contrast to lazy evaluation)
	//def reptree[A]( f: A=>Lst[A])( a :A ) :Tree[A] = Node( a, mapList( reptree(f), f( a ) ) )
	
	//def repstree[A]( f: A=>Stream[A])( a :A ) :STree[A] = SNode( a, f(a).map( repstree(f) ) )
	
			
	// still endless recursion - stream doesnt seem to help here
	def repstree2[A]( f: A=>Stream[A])( a :A ) :STree[A] = {
		
		println( "reptstree called on " + a )
		
		lazy val childs :Stream[A] = f(a)
		
		lazy val childsStream :Stream[STree[A]] = childs.map( repstree2(f) )
		
		println( "creating snode ..." )
		SNode( a, childsStream )
	}
	
	def double[A]( a :A ) :Stream[A] = a #:: a #:: Stream.empty
	
	def succ( i :Int ) : Stream[Int] = (i+1) #:: (i+2) #:: Stream.empty
	
	
	//val stree = repstree2( double[String] )( "A" )
	
	//val stree = repstree2( succ )( 0 )
	
	println( "stree ..." )
	//println( stree )
	
	
	// ------------------
	
	// so heres another approach to lazyness: the subtrees are 'calculated' on demand ...
	// by being representet as a no arg function which calculates the sub trees by function call
	
	trait FTree[+A]
	case class FNode[A]( value :A, childs : () => Lst[FTree[A]] ) extends FTree[A]
	
	val functree =
			FNode( 1, () => 
				FNode( 2,  () => Nl ) +: 
				FNode( 3, () =>  
					FNode( 4,  () => Nl ) +:
					Nl
				) +: 
				Nl	
			)		
	
	def repFtree2[A]( f: A=>Lst[A] )( a :A ) :FTree[A] = {
				
		val childs :Lst[A] = f(a)
		
		val childsFuncs :() => Lst[FTree[A]] = () => mapList( repFtree2(f), childs )
		
		FNode( a, childsFuncs )
	}
	
	
	def repftree[A]( f: A=>Lst[A] )( a :A ) :FTree[A] = FNode( a, () => mapList( repftree(f), f(a) ) )
	
	
	def triple[A]( a :A ) :Lst[A] = a +: a +: a +: Nl
	def succf( i :Int ) : Lst[Int] = (i+1) +: (i+2) +: Nl
	
	val ftree = repFtree2( triple[String] )( "A" )
	
	println( "ftree ..." )
	println( ftree )
	
	def printUntilLevel[A]( level :Int, tree :FTree[A] ) {
		
		if( level > 0 ){
			
			tree match {
				
				case FNode( value, childsFunc ) => {
					
					println( level + ": " + value )
					
					foreach( childsFunc() )( childTree => printUntilLevel( level-1, childTree ) )					
				}
			}			
		}		
	}
	
	printUntilLevel(3, repFtree2( succf )( 0 ) )
	
	type Row = Lst[String]
	type Board = Lst[Row]
	
	def lst[A]( count :Int, a :A ) :Lst[A] = if( count == 0 ) Nl else a +: lst( count-1, a )
	
	def newBoard( rows :Int, colmns :Int ) :Board = lst( rows, lst( colmns, "" ) )
	
	//foldList[A,B]( f :(A,B)=>B, unit:B, lst :Lst[A] )
	def countrow( symb :String, row :Lst[String] ) :Int = foldList[String,Int]( (s,cnt) => if( s == symb ) cnt + 1 else cnt, 0, row )
		
	def count( symb :String, board :Lst[Lst[String]] ) :Int = {
		
		board match {
			
			case Nl => 0
			case +:(r,rs) => countrow( symb, r ) + count( symb, rs )
		}
	}

	def row( i :Int, board :Board ) :Row = board match {
		case Nl => Nl
		case +:(r,rs) if i == 0 => r
		case +:(r,rs) => row( i-1, rs )
	}
	
	//def valueAt( row :Int, colmn :Int, board :Board ) :String = 
	
	
	
	
	
	
	
	
	
	def main( agrs :Array[String] ){
		
	}
}