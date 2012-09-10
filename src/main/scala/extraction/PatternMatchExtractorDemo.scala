package extraction

object PatternMatchExtractorDemo extends Application {

	// An extractor (in its simplest form) is an object P ('Predicate') that defines an unapply method, which simply returns a boolean ...
	object IsInt { def unapply(x: Any): Boolean = x.isInstanceOf[Int] }
	
	// ... it defines a pattern without arguments, like so:    case P() => ...
	def returnIntOrZeroAsAny( x :Any ) : Any = x match{
		case IsInt() => x	// ATTENTION : IsInt without braces wouldn't work! You need to use IsInt() with braces !!!
		case _     => 0
	}
	
	println( "returnIntOrZeroAsAny 32 : " + returnIntOrZeroAsAny( 32 ) )
	println( "returnIntOrZeroAsAny 'hi' : " + returnIntOrZeroAsAny( "hi" ) )
	println( "returnIntOrZeroAsAny false : " + returnIntOrZeroAsAny( false ) )
	
	// ----------------------------------------------------------
	
	// when returning an Option for some type T (which isn't a Product type) ...
	object GetInt { def unapply(x: Any): Option[Int] = if( x.isInstanceOf[Int] ) Some( x.asInstanceOf[Int] ) else None }
	
	// ... you can 'extract' that value of type T, if the extractor results into Some( t ), else (receiving None) the case doesn't match
	def returnIntOrZero( x :Any ) : Int = x match {
		case GetInt( i ) => i
		case _           => 0
	}

	println( "returnIntOrZero 32 : " + returnIntOrZero( 32 ) )
	println( "returnIntOrZero 'hi' : " + returnIntOrZero( "hi" ) )
	println( "returnIntOrZero false : " + returnIntOrZero( false ) )
	
	// ---- here's a product type (Tuple3 for example)
	
	object GetTriple{
	  def unapply(x: Any): Option[Tuple3[_,_,_]] = {
		
	 	  println( "...................." )
	 	  
	 	  if( x.isInstanceOf[Tuple3[_,_,_]] ) Some( x.asInstanceOf[Tuple3[_,_,_]] ) else None
	  }
	}
	
	def returnTripleAsListOrEmptyList( x :Any ) : List[_] = x match {
		case GetTriple( i, j, k ) 	=> i :: j :: k :: Nil
		case _           			=> Nil
	}
	
	println( "returnTripleAsListOrEmptyList (1,1,32) : " + returnTripleAsListOrEmptyList( (1,1,32) ) )
	println( "returnTripleAsListOrEmptyList 'hi' : " + returnTripleAsListOrEmptyList( "hi" ) )
	println( "returnTripleAsListOrEmptyList false : " + returnTripleAsListOrEmptyList( false ) )	
	println( "returnTripleAsListOrEmptyList (true,true,false) : " + returnTripleAsListOrEmptyList( (true,true,false) ) )
	
	
	// -------------------------
	
	def Pattern( ptrn :String ) = new Object{
		def unapply( x: Any ) : Option[String] = ptrn.r.findFirstIn( x.toString )		
	}

//	def startsWith123( x :Any ) = x match {
//		
//		case Pattern( "123" )( res ) => true
//		case _ => false
//	}
	
	def startsWith123( x :Any ) = {
		
		val pattern123 = Pattern( "123" )
		
		x match {	
		  case pattern123( res ) => true
		  case _ => false
	  }
	}
	
	println( "startsWith123 '123Hai' " + startsWith123( "123Hai" ) )
	println( "startsWith123 true " + startsWith123( true ) )
	println( "startsWith123 12345 " + startsWith123( 12345 ) )
}