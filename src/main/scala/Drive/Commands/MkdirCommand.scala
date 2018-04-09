
package Drive.Commands

import Drive.Util.FileDirCompleter
import Drive.Util.MyDir
import Drive.MyDriveState._
import com.google.api.services.drive.model.{File => GFile}
import Drive.MyDriveService._

import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation

import com.google.api.client.http.HttpHeaders
import com.google.api.client.googleapis.json.GoogleJsonError
import com.google.api.client.googleapis.batch.json.JsonBatchCallback

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

@CommandDefinition(name="mkdir", description = "[OPTION]... [DIRECTORY]...")
object MkdirCommand extends Command[CommandInvocation] {
  @cl.Option(shortName = 'h', hasValue = false, description = "display this help and exit")
  private var help: Boolean = false

  @Arguments(completer = classOf[FileDirCompleter])
  private var arguments: java.util.List[String] = null

  private var success: Boolean = true

  override def execute(commandInvocation: CommandInvocation): CommandResult = {
    val argOpts = Option(arguments.asScala)

    if (help) {
      println(commandInvocation.getHelpInfo("mkdir"));
      CommandResult.FAILURE
    } else {
      argOpts match {
        case Some(pathLst) =>
          val gfileLst = ListBuffer[GFile]()
          val batch = service.batch()

          val callback = new JsonBatchCallback[GFile]() {
            override def onFailure(e: GoogleJsonError, responseHeaders: HttpHeaders) = {
              success = false
              System.err.println(e.getMessage())
            }

            override def onSuccess(gfile: GFile, responseHeaders: HttpHeaders) = {
              println("GFile name: " + gfile.getName())
              println("GFile parents: " + gfile.getParents())
              gfileLst += gfile
            }
          }

          pathLst.foreach(path => {
            verifyPath(path) match {
              case Right((lastMileName, lastMyDir, _, _)) =>
                val fileMetadata = new GFile();
                fileMetadata.setName(lastMileName)
                fileMetadata.setMimeType("application/vnd.google-apps.folder")
                fileMetadata.setParents(List(lastMyDir.id).asJava)

                service.files.create(fileMetadata)
                  .setFields("kind, id, name, parents, mimeType, size, trashed, fullFileExtension, fileExtension, starred, modifiedTime, permissions")
                  .queue(batch, callback)
              case Left(_) => ()
            }
          })

          batch.execute()

          gfileLst.foreach(gfile => {
              val myDir = new MyDir(gfile, ListBuffer[GFile](), false)

              myDirById += gfile.getId -> myDir

              gfile.getParents.asScala.foreach(parentId =>
                  myDirById(parentId).childDirs += myDir)
          })

          if (success) {
            CommandResult.SUCCESS
          } else {
            CommandResult.FAILURE
          }
        case None => ()
      }
      

      CommandResult.SUCCESS
    }
  }
}

