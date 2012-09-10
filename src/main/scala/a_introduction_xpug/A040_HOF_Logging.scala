package a_introduction_xpug

import java.util.logging.Logger;
import java.util.logging.Level;

object A040_HOF_Logging {

    
  val logger = Logger.getLogger( "Log Sample" );
  logger.setLevel( Level.INFO )
  
   
   def logMsg( msg :String ) = {
     println( "logMsg( " + msg + " ) called" )
     ">>>" + msg + " <<<"
   }
   
   
   def main( args :Array[String] ){
     
     println( "logDemo" )
     
     logger.finest( logMsg("finest") )
     logger.info( logMsg("info") )
     logger.severe( logMsg("severe") )
          
     if( logger.isLoggable( Level.FINEST ) ) logger.finest( logMsg("if finest") )
     
   }
   
}