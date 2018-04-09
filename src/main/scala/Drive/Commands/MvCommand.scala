
package Drive.Commands
import Drive.Util.FileDirCompleter
import Drive.Util.MyDir
import Drive.MyDriveState._
import Drive.MyDriveService._

import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation
import scala.collection.JavaConverters._
import scala.util.{Try, Success, Failure}
import com.google.api.services.drive.model.{File => GFile}


@CommandDefinition(name="mv", description="moves files or directories")
object MvCommand extends Command[CommandInvocation] {
  @cl.Option(shortName = 'h', hasValue = false, description = "display this help and exit")
  private var help: Boolean = false

  @Arguments(completer = classOf[FileDirCompleter])
  var arguments: java.util.List[String] = null

  private var success: Boolean = true

  private def mvHelper(destMyDir: MyDir, elem: Either[GFile, MyDir]): Unit = {
    // TODO: We're basically checking Either twice so fix that
    val gfile = if (elem.isLeft) elem.left.get else elem.right.get.gfile
    val oldParents = new StringBuilder()
    Option(gfile.getParents().asScala).foreach(
      _.foreach(parent => {
        oldParents ++= parent
        oldParents ++= ","
      }))

    // TODO: Maybe execute all the commands at once or something?
    // Currently there's a slight delay, probably due to the fact
    // that the network is slow compared to memory
    val tryUpdate = util.Try(service.files.update(gfile.getId, null)
      .setAddParents(destMyDir.gfile.getId)
      .setRemoveParents(oldParents.toString())
      .setFields("id, parents")
      .execute())

    /* 
     * If operation was successful
     * Delete references to old GFile or MyDir
     * Create new references for the destination MyDir
     */
    tryUpdate match {
      case Success(newGFile) =>
        elem match {
          case Left(gfile) =>
            RmCommand.rmGFile(gfile)
            destMyDir.childFiles += newGFile
          case Right(myDir) =>
            RmCommand.rmMyDirTree(myDir)

            val newMyDir = new MyDir(newGFile, myDir.childFiles, false)
            newMyDir.childDirs ++= myDir.childDirs

            destMyDir.childDirs += newMyDir
        }
      case Failure(failure) =>
        success = false
        println("Failure to move file/directory")
        println(failure)
    }
  }

  override def execute(commandInvocation: CommandInvocation): CommandResult = {
    val argsOpt = Option(arguments.asScala)

    if (help) {
      println(commandInvocation.getHelpInfo("mv"));
      CommandResult.SUCCESS
    } else {
      argsOpt match {
        case Some(pathLst) =>
          val verifiedLst = verifyList(pathLst.toList, true).map(x => x._1) 
          val verifiedSrcLst = verifiedLst.reverse.tail
  
          // If even one of the arguments doesn't check out
          // Or if one of the arguments other than the last is the root dir
          // Don't do anything
          if (verifiedLst.exists(_.isEmpty) || verifiedSrcLst.exists(elem => elem.get.isRight && elem.get.right.get.isRootDir)) {
            // TODO: Specify which argument doesn't check out
            println("Error in one of the arguments")
            CommandResult.FAILURE
          } else {
            val destDirEither = verifiedLst.reverse.head.get

            if (destDirEither.isRight) {
              val destMyDir = destDirEither.right.get
              verifiedSrcLst.map(elem => { mvHelper(destMyDir, elem.get) })
              if (success) {
                CommandResult.SUCCESS
              } else {
                CommandResult.FAILURE
              }
            } else {
              println("mv: last target is not a directory")
              CommandResult.FAILURE
            }
          }
        case None =>
          println("mv: requires at least one argument")
          CommandResult.FAILURE
      }
    }
  }
}

