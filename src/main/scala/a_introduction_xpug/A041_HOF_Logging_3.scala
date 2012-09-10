package a_introduction_xpug

import java.util.logging.Logger;
import java.util.logging.Level;

object A041_HOF_Logging_3 {

   val logger = Logger.getLogger( "Log Sample" );
  logger.setLevel( Level.INFO )
  
  
  
   val log = ( logger :Logger, level :Level ) => {
   
      val isLog = logger.isLoggable( level )
   
     ( msg : () => String ) => if( isLog ) logger.log( level, msg() )
   }
  
   
   
   
   
   
   def logMsg( msg :String ) = {
     println( "logMsg( " + msg + " ) called" )
     ">>>" + msg + " <<<"
   }
   
   
   def main( args :Array[String] ){
     
     println( "logDemo ..." )
     
     val finest = log( logger, Level.FINEST )
     
     
     finest( () => logMsg( "finest" ) )
     
     
   }
}