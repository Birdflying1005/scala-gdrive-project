
package Drive.Commands

import Drive.Util.FileDirCompleter
import Drive.Util.MyDir
import Drive.MyDriveState._
import Drive.MyDriveService._
import scala.collection.mutable.{ListBuffer, Queue}
import scala.collection.JavaConverters._
import scala.util.{Try, Success, Failure}

import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation

import com.google.api.services.drive.model.{File => GFile}
import com.google.api.client.http.HttpHeaders
import com.google.api.client.googleapis.json.GoogleJsonError
import com.google.api.client.googleapis.batch.json.JsonBatchCallback


@CommandDefinition(name="rm", description="deletes files or directories")
object RmCommand extends Command[CommandInvocation] {
  @cl.Option(shortName = 'h', hasValue = false, description = "display this help and exit")
  private var help: Boolean = false

  @cl.Option(shortName = 'f', hasValue = false, description = "never prompt")
  private var force: Boolean = false

  @Arguments(completer = classOf[FileDirCompleter])
  var arguments: java.util.List[String] = null

  /**
   * Checks GFile and deletes from parent directories if multiple parents
   * If myDirOpt is provided, excludes that as that will be deleted anyways
   *
   * @param childGFile GFile to delete
   * @param myDirOpt parent directory to exclude from deletion
   *
   */
  def rmGFile(childGFile: GFile): Unit = {
    Option(childGFile.getParents.asScala).foreach(
      parents =>
        parents.foreach(
          parentId => Try(myDirById(parentId)).foreach(
            parentMyDir => parentMyDir.childFiles -= childGFile))
    )
  }

  /**
   * Checks myDir and deletes from parent directories
   * @param myDir directory entry to delete
   *
   */
  private def rmMyDir(myDir: MyDir): Unit = {
    Option(myDir.gfile.getParents.asScala).foreach(
      parents => parents.foreach(
        parentId => Try(myDirById(parentId)).foreach(
          parentMyDir => parentMyDir.childDirs -= myDir
        )
      )
    )

    myDirById -= myDir.id
  }

  /**
   * Helper function
   *
   * Runs BFS through the directory structure
   * Adds each directory to list
   * Reverses list (So deepest dirs first)
   * Deletes directories in reversed list
   *
   */
  def rmMyDirTree(myDir: MyDir): Unit = {
    val dirQueue = Queue[MyDir]()
    var childDirs = ListBuffer[MyDir]()

    dirQueue += myDir
    while (!dirQueue.isEmpty) {
      val deqMyDir = dirQueue.dequeue()

      childDirs += deqMyDir
      dirQueue ++= deqMyDir.childDirs
    }

    childDirs = childDirs.reverse
    childDirs.foreach(childMyDir => {
      childMyDir.childFiles.foreach(childGFile => rmGFile(childGFile))
      rmMyDir(childMyDir)
    })

    rmMyDir(myDir)
  }

  def deleteVerifiedList(verifiedLst: List[Either[GFile, MyDir]]): Unit = {
    val successLst = ListBuffer[Boolean]()
    val batch = service.batch()

    val callback1 = new JsonBatchCallback[Void]() {
      override def onFailure(e: GoogleJsonError, responseHeaders: HttpHeaders) = {
        successLst += false
        System.err.println(e.getMessage())
      }

      override def onSuccess(void: Void, responseHeaders: HttpHeaders) = {
        successLst += true
      }
    }

    val callback2 = new JsonBatchCallback[GFile]() {
      override def onFailure(e: GoogleJsonError, responseHeaders: HttpHeaders) = {
        successLst += false
        System.err.println(e.getMessage())
      }

      override def onSuccess(gfile: GFile, responseHeaders: HttpHeaders) = {
        successLst += true
      }
    }

    verifiedLst.foreach(elem => {
      val fileId = elem match {
        case Left(gfile) =>
          gfile.getId
        case Right(myDir) =>
          myDir.id
      }

      if (force) {
        service.files.delete(fileId)
            .queue(batch, callback1)
      } else {
        val newGFile = new GFile()
        newGFile.setTrashed(true)
        service.files.update(fileId, newGFile)
            .queue(batch, callback2)
      }
    })

    batch.execute()

    val combinedLst = successLst.toList zip verifiedLst

    // This is assuming the requests are carried out in the order specified
    combinedLst.foreach({ case (success, elem) => {
      if (success) {
        elem match {
          case Left(gfile) =>
            rmGFile(gfile)
          case Right(myDir) =>
            rmMyDirTree(myDir)
        }
      }
    }})
  }


  override def execute(commandInvocation: CommandInvocation): CommandResult = {
    val argsOpt = Option(arguments.asScala)

    if (help) {
      println(commandInvocation.getHelpInfo("rm"));
      CommandResult.SUCCESS
    } else {
      argsOpt match {
        case Some(pathLst) =>
          val verifiedLst = verifyList(pathLst.toList, false).map(x => x._1)
  
          // If even one of the path arguments doesn't check out
          // Don't do anything
          if (verifiedLst.exists(_.isEmpty)) {
            // TODO: Specify which argument doesn't check out
            println("Error in one of the arguments")
            CommandResult.FAILURE
          } else {
            deleteVerifiedList(verifiedLst.map(_.get))
            CommandResult.SUCCESS
          }
        case None =>
          println("rm: requires at least one argument")
          CommandResult.FAILURE
      }
    }
  }
}


