package Drive.Util

import org.jboss.aesh.cl.completer.OptionCompleter
import org.jboss.aesh.console.command.completer.CompleterInvocation
import scala.collection.JavaConverters._
import Drive.MyDriveService._

class FileDirCompleter extends OptionCompleter[CompleterInvocation] {
  /**
    * Provides all possible completer values given completerInvocation
    * @param completerInvocation instance that stores completion/completer values
    *
    * For some reason the completion doesn't work sometimes but I've tested
    * and the correct values are being added to completerInvocation
    */
  override def complete(completerInvocation: CompleterInvocation): Unit = {
    val path: String = completerInvocation.getGivenCompleteValue

    verifyPath(path) match {
      case Right(lastTuple) => 
        val lastDirName = lastTuple._1
        val lastMyDir = lastTuple._2

        val possibilities: List[String] = lastMyDir.childDirs.map(_.name).filter(_.startsWith(lastDirName)).toList :::
          lastMyDir.childFiles.map(_.getName).filter(_.startsWith(lastDirName)).toList

        completerInvocation.addAllCompleterValues(possibilities.asJavaCollection)
      case Left(failure) => 
        failure match {
          case EmptyFollowingPathFailure(dir) => 
            val possibilities: List[String] = dir.childDirs.map(_.name).toList :::
              dir.childFiles.map(_.getName).toList

            completerInvocation.addAllCompleterValues(possibilities.asJavaCollection)
          case _ => ()
        }
    }
  }
}


