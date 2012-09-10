package cleancode


object FindRoute extends Application{

	type Point = Char
	
	type Path = (Point,Point)
	
	type Map = List[Path]
	
	type Route = List[Path]
	
	
	def from( point :Point, map :Map ) : Map = map.filter( _._1 == point )

	
	
	def findRoute( start :Point, end :Point, map :Map ) :Route = {
						
		var route :Route = (start,start) :: Nil		
		var deadEnd :Map = Nil
		
		var childs = from( start, map ).filter( path => !deadEnd.contains( path ) && !route.contains( path ) )
		
		while( !childs.isEmpty && route.head._2 != end ){
			
			route = childs.iterator.next :: route
			
			childs = from( route.head._2, map ).filter( path => !deadEnd.contains( path ) && !route.contains( path ) )
			
			while( childs.isEmpty && !route.isEmpty ) {
				
				deadEnd = route.head :: deadEnd
				route = route.tail
				
				childs = if( !route.isEmpty ) from( route.head._2, map ).filter( path => !deadEnd.contains( path ) && !route.contains( path ) ) else Nil
			}
		}
		if( !route.isEmpty ) route.reverse.tail else route
	}
	
	
	def findRoute2( start :Point, end :Point, space :Map ) :Route = {
						
		var route :Route = (start,start) :: Nil		
		var deadEnd :Map = Nil
		
		while( !route.isEmpty && route.head._2 != end ){
			
			var childs = from( route.head._2, space ).filter( path => !deadEnd.contains( path ) && !route.contains( path ) )

			if( childs.isEmpty && !route.isEmpty ) {

				deadEnd = route.head :: deadEnd
				route = route.tail
			}
			else{
				route = childs.iterator.next :: route
			}
		}
		
		if( !route.isEmpty ) route.reverse.tail else route
	}
	
	
	
		
	def findRoute3( start :Char, end :Char, pths :List[(Char,Char)] ) :List[(Char,Char)] = {
						
		var rte = (start,start) :: Nil		
		var de :List[(Char,Char)] = Nil
		
		while( !rte.isEmpty && rte.head._2 != end ){
			
			var childs = from( rte.head._2, pths ).filter( pth => !de.contains( pth ) && !rte.contains( pth ) )

			if( childs.isEmpty && !rte.isEmpty ) {

				de = rte.head :: de
				rte = rte.tail
			}
			else{
				rte = childs.iterator.next :: rte
			}
		}
		
		rte.reverse.tail
	}
	
	
	//import java.util.Stack
	import scala.collection.mutable.Stack
	import scala.collection.mutable.HashSet
	import scala.collection.mutable.Set
	
	case class WayPoint( id :Char )
	case class WayPath( startPoint :WayPoint, endPoint :WayPoint )
	
	def pathsFrom( startPoint :WayPoint, map :Set[WayPath] ) : Set[WayPath] = map.filter( path => path.startPoint == startPoint )
	
	implicit def toRichSet[A]( set :Set[A] ) = new {
		def without[MA <% Seq[A]]( xs :MA ) = set.filter( elem => !xs.contains( elem) )
	}
	
	
	def findRoute( start :WayPoint, target :WayPoint, map :Set[WayPath] ) :Sequence[WayPath] = {
						
		var route = new Stack[WayPath] 
		var deadEnds :Set[WayPath] = new HashSet[WayPath]
		
		route push WayPath( start, start )
		
		while( !route.isEmpty && route.top.endPoint != target ){
			
			var continuingPaths = pathsFrom( route.top.endPoint, map ).without( deadEnds.toSeq ).without( route )

			if( continuingPaths.isEmpty && !route.isEmpty ) {

				deadEnds += route.pop
			}
			else{
				route push continuingPaths.first
			}
		}
		
		route.reverse

	}	
	
//	import scala.collection.mutable.Stack
//	
//	val s = new Stack
	
	
	

	var map :Map = 
	( ('A':Point,'B':Point) :Path ) ::
	 ( ('B':Point,'C':Point) :Path ) ::
	( ('B':Point,'E':Point) :Path ) ::
	( ('C':Point,'A':Point) :Path ) ::
	( ('C':Point,'D':Point) :Path ) ::
	( ('D':Point,'E':Point) :Path ) ::
	 ( ('E':Point,'B':Point) :Path ) :: Nil
	
	println( findRoute3( 'A' :Point, 'E':Point, map ) )
	
	
	
}