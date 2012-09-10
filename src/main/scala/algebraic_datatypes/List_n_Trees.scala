package algebraic_datatypes

object List_n_Trees {

	
	sealed abstract class IntList
	case object EmptyIntList extends IntList
	case class NonEmptyIntList( hd :Int, tl :IntList ) extends IntList
	
	val ilst = NonEmptyIntList( 1, NonEmptyIntList( 2, NonEmptyIntList( 2, EmptyIntList ) ) )

	// ----------------
	
	// non polymorph
	sealed abstract class NPLst[A]
	case object NPNl extends NPLst[Nothing]
	case class NPCons[A]( x :A, tail :NPLst[A] ) extends NPLst[A]	
	
	//val x : NPLst[Int] = NPCons( 1, NPCons( 2, NPCons( 3, NPNl ) ) )
//	val xy = NPCons( "1", NPCons( "2", NPCons( "3", NPNl[String] ) ) )
	
	// --------
	
	sealed abstract class Lst [+A]{
		def +:[S >: A] ( x :S ) :Lst[S] = new +:( x, this )
	}
	case object Nl extends Lst[Nothing]
	case class +:[A]( x :A, tail :Lst[A] ) extends Lst[A]
	
	def +:[A]( x :A, xs :Lst[A] ) = x +: xs
	
	//case class Cons[A]( x :A, tail :Lst[A] ) extends Lst[A]


	def last[A]( lst :Lst[A] ) : Option[A] = lst match {
		
		case Nl => None
		case x +: Nl => Some( x )
		case _ +: x +: xs => last( x +: xs ) 
		
//		case Cons( _, Cons( x, xs ) ) => last(  Cons( x, xs ) )
		//case _ Cons x Cons xs  => last(  Cons( x, xs ) )  
	}
	
	
	
//	object +: {
//		def unapply[A]( x: Lst[A] ) : Option[(A,Lst[A])] = x match {
//			case Cons( hd, tl ) => Some( (hd,tl) )
//			case _ => None
//		}
//	}
	
	
	
//	val length2 : Lst[_] => Int = _ match{
//		
//		case Nl => 0
//		case head +: tail => 1 + length2( tail )
//		
//	}
//	 	  
			
	def empty( lst :Lst[_] ) = lst == Nl
	
	def head[A]( lst :Lst[A] ) : A = lst match {
	  case a +: _ 	=> a
	  case _ 		=> error( "no head" )
	}
	
	def headSafe[A]( lst :Lst[A] ) : Option[A] = lst match {
	  case a +: _ 	=> Some(a)
	  case _ 		=> None
	}	
	
	def tail[A]( lst :Lst[A] ) : Lst[A] = lst match{
	  case _ +: as 	=> as
	  case _		=> lst
	}
	
	def init[A]( lst :Lst[A] ) : Lst[A] = lst match {
	  case Nl 				=> Nl
	  case a +: last +: Nl	=> a +: Nl
	  case a +: as 			=> a +: init( as )
	}
	
	
	def append[A]( a :A, lst :Lst[A] ) : Lst[A] = insertAt( length( lst ), a, lst )
	
	def reverse[A]( lst :Lst[A] ) : Lst[A] = lst match {
	  case Nl => Nl
	  case a +: as => append( a, reverse( as ) )
	}
	
	def reverse2[A]( lst :Lst[A] ) : Lst[A] = {
	  
	  def rev( lxs :Lst[A], accu :Lst[A] ) : Lst[A] = ( lxs, accu ) match {
	    case ( Nl, acc ) 		=> acc
	    case ( x +: Nl, acc ) 	=> x +: acc
	    case ( x +: xs, acc )	=> rev( xs, x +: acc )
	  }
	  rev( lst, Nl )
	}
	
	
	def flatten[A]( xxs :Lst[Lst[A]] ) :Lst[A] = xxs match {
	  case Nl => Nl
	  case x +: xs => concat( x, flatten( xs ) )
	}	
	
	
	def take[A]( count :Int, lst :Lst[A] ) :Lst[A] = ( count, lst ) match {
	  case ( _ , Nl ) 		=> Nl
	  case ( 0 , _ )		=> Nl
	  case ( i, a +: as )	=> a +: take( i-1, as )
	}
	
	def takeWhile[A]( p :A => Boolean, lst :Lst[A] ) :Lst[A] = lst match {
	  case Nl 					=> Nl
	  case a +: _  if !p( a )	=> Nl
	  case a +: as  			=> a +: takeWhile( p, as )	  
	}
	
	def drop[A]( count :Int, lst :Lst[A] ) :Lst[A] = ( count, lst ) match {
	  case ( _ , Nl ) 		=> Nl
	  case ( 0 , as )		=> as
	  case ( i, a +: as )	=> drop( i-1, as )
	}
	
	def dropWhile[A]( p :A => Boolean, lst :Lst[A] ) :Lst[A] = lst match {
	  case Nl							=> Nl
	  case all @ (a +: as)  if !p( a )	=> all
	  case _ +: as						=> dropWhile( p, as )
	}
	
	
	
	def zip[A,B]( as :Lst[A], bs :Lst[B] ) :Lst[(A,B)] = (as, bs) match {
			
		case( Nl, _ ) => Nl
		case( _, Nl ) => Nl
		case( a +: at, b +: bt ) => ( a,b ) +: zip( at, bt )
	}
	
	def zipWith[A,B,C]( f :(A,B)=>C, as :Lst[A], bs :Lst[B] ) : Lst[C] = ( as, bs ) match {
		
		case( Nl, _ ) | ( _, Nl ) => Nl
		case( a +: at, b +: bt ) => f(a,b) +: zipWith( f, at, bt )
	}
	
	def repeat[A]( a :A , times :Int ) :Lst[A] = if( times == 0 ) Nl else a +: repeat( a , times - 1 )
	def lst[A]( count :Int, a :A ) :Lst[A] = if( count == 0 ) Nl else a +: lst( count-1, a )
	
	def interval( start :Int, end :Int, step :Int ) :Lst[Int] = ( start, end ) match {
	  case( s, e ) if s > e		=> Nl
	  case( s, e )				=> s +: interval( s + step, e, step )
	}
	
	
	//zapp :: [a -> b ] -> [a] -> [b]
	def zapp[A,B]( funcs :Lst[A=>B] )( as :Lst[A] ):Lst[B] = ( funcs, as ) match {
		
		case ( f +: fs, x +: xs ) 	=> f(x) +: zapp( fs )( xs )
		case ( _, _ ) 				=> Nl
	}
	
	
	val max = ( lst :Lst[Int] ) => foldList( (max:Int, i:Int) => if( i > max ) i else max, 0, lst )
	
	val length : Lst[_] => Int 
	   = _ match {
		
		case Nl => 0
		case ( _ +: tl ) => 1 + length( tl )
	}
	
	def zipWith2[A,B,C]( func :A=>B=>C )( as :Lst[A] )( bs :Lst[B] ) : Lst[C] = {

		val funcs = repeat( func , max( length( as ) +: length( bs ) +: Nl ) )
				
		zapp( zapp( funcs )( as ) )( bs )
	}
	
	def zipWith3[A,B,C,D]( func :A=>B=>C=>D )( as :Lst[A] )( bs :Lst[B] )( cs :Lst[C] ) : Lst[D] = {

		val funcs = repeat( func , max( length( as ) +: length( bs ) +: Nl ) )
				
		zapp( zapp( zapp( funcs )( as ) )( bs ) )( cs )
	}	
	
		def mapList[A,B]( f :A=>B, lst :Lst[A] ) :Lst[B] = lst match {
		
		case Nl => Nl
		case a +: as => f( a ) +: mapList( f, as )
	}
	
	def foreach[A]( lst :Lst[A])( f :A=>Unit ){
		
		lst match{
			
			case Nl => Unit
			case a +: as => f(a); foreach( as)( f )
		}
	}
	
	
	def foldList[A,B]( f :(A,B)=>B, unit:B, lst :Lst[A] ) :B = lst match {
		
		case Nl => unit
		case hd +: tl => f( hd, foldList( f, unit, tl ) )
	}
	
	def concat[A]( xs :Lst[A], ys :Lst[A] ) :Lst[A] = foldList( +:[A] _, ys, xs )
	
	
	def concatConventional[A,B>:A]( lxs :Lst[A], lys :Lst[B] ) :Lst[B] = ( lxs, lys ) match {
	  case ( Nl, ys )		=> ys
	  case ( x +: xs, ys )	=> x +: concatConventional( xs, ys )
	}
	
	
	println( "concat 1,2,3 ++ 6,7,8 : " + show( concat(  1 +: 2 +: 3 +: Nl, 6 +: 7 +: 8 +: Nl ) ) )
	println( "concat 1,2,3 ++ 'a','b','c' : " + show( concat(  1 +: 2 +: 3 +: Nl, "a" +: "b" +: "c" +: Nl ) ) )
	
	
	// -------------
	
	def not[A]( p :A=>Boolean ) :A=>Boolean = (a:A) => !p(a)
		
	def partitionOnFilter[A]( p:A=>Boolean, lst :Lst[A] ) : (Lst[A],Lst[A]) = ( filter( p, lst ), filter( not(p), lst ) )
		  
	// ---
	

//	def partition_[A]( p:A=>Boolean, lst :Lst[A] ) : (Lst[A],Lst[A]) = {
//	  
//		def part[A]( p:A=>Boolean, lst :Lst[A], acc :(Lst[A],Lst[A]) ) : (Lst[A],Lst[A]) = (lst,acc) match {
//		  case (Nl, accu) 					=> accu
//		  case (a +: as, (ts,fs) )  if p(a)	=> part( p, as, ( a +: ts, fs ) )
//		  case (a +: as, (ts,fs) )  		=> part( p, as, ( ts, a +: fs ) )
//		}
//		
//		part( p, lst, (Nl,Nl) )
//	}
	
	def partition[A]( p :A=>Boolean, lst :Lst[A] ) : (Lst[A],Lst[A]) = lst match {
	  case Nl 		=> (Nl,Nl)
	  case a +: as  => 	val (ts,fs) = partition( p, as ) 
	  					if( p(a) ) ( a +: ts, fs ) else ( ts, a +: fs )
	  					
//	  case a +: as  if p(a)		=> val (ts,fs) = partition( p, as ); ( a +: ts, fs )
//	  case a +: as  			=> val (ts,fs) = partition( p, as ); ( ts, a +: fs )	  	  	  
	}
	
	// ---
		
	def select[A]( p :A=>Boolean )( a :A, acc :( Lst[A], Lst[A] ) ) : ( Lst[A], Lst[A] ) = ( p( a ), acc ) match {
	  case ( true, (ts,fs) ) => ( a +: ts, fs )
	  case ( _ , (ts,fs) )	 => ( ts, a +: fs )
	}	
	
	def partitionOnFold[A]( p:A=>Boolean, lst :Lst[A] ) : (Lst[A],Lst[A]) = foldList( select( p )_, (Nl,Nl), lst )	
	  
	// -------------
	
	def splitAt[A]( pos :Int, lst :Lst[A] ) : (Lst[A],Lst[A]) = ( pos, lst) match {
	  case( _ , Nl ) 	 => (Nl,Nl)
	  case( 0, as ) 	 => ( Nl, as )
	  case( i, a +: as ) => val (p1,p2) = splitAt( i-1, as ); ( a +: p1, p2 )
	}	
	
//	def splitAtInner[A]( pos :Int, lst :Lst[A] ) : (Lst[A],Lst[A]) = {
//		
//	    def split[A]( pos :Int, lst :Lst[A], acc :Lst[A] ) : (Lst[A],Lst[A]) = ( pos, lst ) match {
//		  case( _ , Nl ) 	 => (acc,Nl)
//		  case( 0, a +: as ) => ( a +: acc, as )
//		  case( i, a +: as ) => split( i-1, as, a +: acc )
//		}
//		
//		split( pos, lst, Nl )
//	}
	
	def splitAt2[A]( pos :Int, lst :Lst[A] ) : (Lst[A],Lst[A]) = ( take( pos, lst ), drop( pos, lst ) )
		
	// -------------
	
	def span[A]( p :A=>Boolean, lst :Lst[A] ) = ( takeWhile( p, lst ), dropWhile( p, lst ) )
	
	
	
	// -------------
	
	def filter[A]( p :A=>Boolean, lst :Lst[A] ) :Lst[A] = lst match {
	  case Nl 				=> Nl
	  case a +: as if p(a) 	=> a +: filter( p, as )
	  case a +: as			=> filter( p, as )
	}
	
	
	// ---------------------
	
	def intersperse[A,B>:A]( b:B, lst :Lst[A] ) : Lst[B] = lst match {
	  case Nl  				=> Nl
	  case last @ (a +: Nl)	=> last
	  case a +: as 			=> a +: b +: intersperse( b, as )
	}
	
	
	// ---------------------
	
	def updateAt[A,B>:A]( pos :Int, newVal :B, lst :Lst[A]  ) :Lst[B] = (lst, pos ) match {
		
		case ( Nl, _ ) => newVal +: Nl
		
		case ( a +: as, 0 ) => newVal +: as
		
		case ( a +: as, i ) => a +: updateAt( i-1, newVal, as )
	}
	
	def insertAt[A,B>:A](  pos :Int, b :B, lst :Lst[A] ) :Lst[B] = (lst,pos) match {
		
		case ( Nl, _ )		=>	b +: Nl
		
		case( as, 0 )		=> b +: as
		
		case( a +: as, i  )	=> a +: insertAt( i-1, b, as )
	}
	
	
	def insertAtNonCovariant[A](  pos :Int, b :A, lst :Lst[A] ) :Lst[A] = (lst,pos) match {
		
		case ( Nl, _ )		=>	b +: Nl
		
		case( as, 0 )		=> b +: as
		
		case( a +: as, i  )	=> a +: insertAt( i-1, b, as )
	}
	
	
	def show( lst :Lst[_] ) :String = lst match {
		
		case Nl 		=> "Nl"
		case x +: xs 	=>  x + " +: " + show( xs )
	}
	
	def element[A]( x :A, lst :Lst[A] ) : Boolean = lst match {
		case Nl 		=> false
		case a +: _ if(a==x) 	=> true
		case _ +: as => ( element( x, as ) )
	}
	
	def elemAt[A]( pos :Int, lst :Lst[A] ) :Option[A] = ( lst, pos ) match {
		
		case( Nl, _ ) 		=> None
		case( a +: _, 0 ) 	=> Some( a )
		case( _ +: as, i )	=> elemAt( i-1, as )
	}
	
	def remove[A]( x :A, lst :Lst[A] ) :Lst[A] = lst match{
		
		case Nl 					=> Nl
		
		case a +: as if( a == x )	=> remove( x, as )
		
		case a +: as				=> a +: remove( x, as )
	}
	
	def removeAt[A]( pos :Int, lst :Lst[A] ) :Lst[A] = ( lst, pos) match {
		
		case ( Nl, _ )		=> Nl
		
		case ( a +: as, 0 )	=> as
		
		case ( a +: as, i ) => a +: removeAt( i-1, as )
	}
	
	
	def allTrue( lst :Lst[Boolean] ) : Boolean = lst match {
	  case Nl 		=> true
	  case a +: as 	=> a == true && allTrue( as )
	}
	

	def inAscendingOrder( lst :Lst[Int] ) =	allTrue( mapList( (pair:(Int,Int)) => pair._1 < pair._2, zip( lst, tail( lst ) ) ) )  
	
	
	
	
	
	def indexOf[A]( a :A, lst :Lst[A] ) = {	  
	   mapList( (x:(A,Int))=> x._2 , filter( (x:(A,Int))=> x._1 == a , zip( lst, interval( 0, length( lst ), 1 ) ) )	)  
	}
	
	
	val lst :Lst[Int] = 1 +: 2 +: 3 +: 4 +: 2 +: 5 +: Nl
	
	println( "orig : " + show( lst ) )
	println( "empty lst : " + empty( lst ) )
	println( "empty Nl : " + empty( Nl ) )
	println( "head : " + head( lst ) )
	println( "head (safe): " + headSafe( lst ) )
	println( "head Nl (safe): " + headSafe( Nl ) )
	println( "tail : " + show( tail( lst ) ) )
	println( "updateAt 2 30 : " + show( updateAt( 2, 30, lst ) ) )
	println( "updateAt 10 30 : " + show( updateAt( 10, 30, lst ) ) )
	println( "updateAt -10 30 : " + show( updateAt( -10, 30, lst ) ) )
	println( "insertAt 2 40 :" + show( insertAt( 2, 40, lst ) ) )
	println( "append 99 : " + show( append( 99, lst ) ) )
	println( "append 99 on Nl : " + show( append( 99, Nl ) ) )
	println( "elem 'a' : " + element( "a", lst ) )
	println( "elem 4 : " + element( 4, lst ) )
	println( "elem 50 : " + element( 50, lst ) )
	println( "elem at 2 : " + elemAt( 2, lst ) )
	println( "elem at -2 : " + elemAt( -2, lst ) )
	println( "elem at 11 : " + elemAt( 11, lst ) )
	println( "remove 'a' : " + show( remove( "a", lst ) ) )	// why no compile error ??? "a" is of type String, but it need to be of type Int !? 
	println( "remove 4 : " + show( remove( 4, lst ) ) )
	println( "remove 2 : " + show( remove( 2, lst ) ) )
	println( "remove 50 : " + show( remove( 50, lst ) ) )
	println( "remove at 2 : " + show( removeAt( 2, lst ) ) )
	println( "remove at 10 : " + show( removeAt( 10, lst ) ) )
	println( "last : " + last( lst ) )
	println( "init : " + show( init( lst ) ) )
	println( "reverse : " + show( reverse( lst ) ) )
	println( "reverse2 : " + show( reverse2( lst ) ) )
	println( "take 3 : " + show( take( 3, lst ) ) )
	println( "take 0 : " + show( take( 0, lst ) ) )
	println( "take 10 : " + show( take( 10, lst ) ) )
	println( "take while < 3 : " + show( takeWhile( (_:Int) < 3, lst ) ) )
	println( "take while all true : " + show( takeWhile( (x:Int) => true, lst ) ) )
	println( "take while all false : " + show( takeWhile( (x:Int) => false, lst ) ) )
	println( "drop 3 : " + show( drop( 3, lst ) ) )
	println( "drop 0 : " + show( drop( 0, lst ) ) )
	println( "drop 10 : " + show( drop( 10, lst ) ) )
	println( "drop while < 3 : " + show( dropWhile( (_:Int) < 3, lst ) ) )
	println( "drop while all true : " + show( dropWhile( (x:Int) => true, lst ) ) )
	println( "drop while all false : " + show( dropWhile( (x:Int) => false, lst ) ) )		
	println( "zip( lst, tail( lst ) ) : " + show( zip( lst, tail( lst ) ) ) )
	println( "inAscendingOrder( lst ) ) : " + inAscendingOrder( lst ) )
	println( "inAscendingOrder( 1 +: 2 +: 3 +: Nl ) ) : " + inAscendingOrder( 1 +: 2 +: 3 +: Nl ) )
	println( "interval 1 - 10 +1 : " + show( interval( 1, 10, 1 ) ) )
	println( "interval 7 - 20 +3 : " + show( interval( 7, 20, 3 ) ) )
	println( "indexOf 2 : " + show( indexOf( 2, lst ) ) )
	println( "indexOf 3 : " + show( indexOf( 3, lst ) ) )
	println( "indexOf 99 : " + show( indexOf( 99, lst ) ) )
	println( "concatConventional lst with 99 +: 100 +: 101 : " + show( concatConventional( lst, 99 +: 100 +: 101 +: Nl ) ) )
	println( "concat (by fold) lst with 99 +: 100 +: 101 : " + show( concat( lst, 99 +: 100 +: 101 +: Nl ) ) )
	println( "flatten lst, 111+:222+:333, lst : " + show( flatten( lst +: ( 111 +: 222 +: 333 +: Nl) +: lst +: Nl ) ) )
	println( "partition on filter : " + partitionOnFilter[Int]( _ >= 3, lst ) )
	println( "partition : " + partition[Int]( _ >= 3, lst ) )
	println( "partition on fold : " + partitionOnFold[Int]( _ >= 3, lst ) )
	println( "intersperse 'a' : " + show( intersperse( "a", lst ) ) )
	println( "intersperse on Nl : " + show( intersperse( "a", Nl ) ) )
	println( "intersperse on 1 : " + show( intersperse( "a", 1 +: Nl ) ) )
	println( "splitAt 2 : " + splitAt( 2, lst ) )
	println( "splitAt2 2 : " + splitAt2( 2, lst ) )
	println( "repeat succ : " + show( repeat( (x:Int) => x+1, 3 ) ) )
	
val ints :Lst[Int] = 1 +: 2 +: 3 +: 4 +: 5 +: Nl
val (firstTwo,lastThree) = splitAt2( 2, ints )
println( "splitted : " + show( firstTwo ) + " - " + show( lastThree ) )


	// -----------------------------------------------------------------------------------------
	
	trait Tree[+A]
	case class Node[A]( value :A, childs :Lst[Tree[A]] ) extends Tree[A]

	
	val tree =
			Node( 1, 
				Node( 2, Nl ) +: 
				Node( 3,  
					Node( 4, Nl ) +:
					Nl
				) +: 
				Nl	
			)
	
	
	def foldTree[A,B]( f :(A,B)=>B, g :(B,B)=>B, unit:B, tree :Tree[A] ) :B = tree match{
		
		case Node( value, childs ) => f( value, foldTree( f,g,unit, childs ) )
	}
			
			
	def foldTree[A,B]( f :(A,B)=>B, g :(B,B)=>B, unit:B, subtrees :Lst[Tree[A]] ) :B = subtrees match{
		
		case Nl => unit
		case subtree +: rest  => g( foldTree( f,g,unit, subtree ), foldTree( f,g,unit, rest ) )
	}
	
	def mapTree[A,B]( f :A=>B, tree :Tree[A] ) :Tree[B] = tree match{
		
		case Node( value, childs ) =>  Node( f(value), mapTree( f, childs ) )
	}
	
	def mapTree[A,B]( f :A=>B, subtrees :Lst[Tree[A]] ) :Lst[Tree[B]] = subtrees match {
		
		case Nl => Nl
		case subtree +: rest  => mapTree( f, subtree ) +: mapTree( f, rest )
	}
	
	
	
	val add = (_:Int) + (_:Int)
	
	println( "sumtree : " + foldTree( add,add,0, tree ) )
	
	
	
	def values[A]( tree :Tree[A] ) :Lst[A] = foldTree( +:[A] _, concat[A] _, Nl, tree )
 
	println( "values : " + values( tree ) )
	
	

	println( "map tostring : " + mapTree( (x:Int) => "<"+x.toString+">", tree   ) )
	
	
	def main( args :Array[String]){
		
	}
}