
package Drive.Commands

import scala.collection.JavaConverters._
import Drive.Util.FileDirCompleter
import Drive.MyDriveService._
import Drive.MyDriveState._
import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation
import org.jboss.aesh.console.Prompt


@CommandDefinition(name="cd", description="changes directory")
object CdCommand extends Command[CommandInvocation] {
  @cl.Option(shortName = 'h', hasValue = false, description = "display this help and exit")
  private var help: Boolean = false

  @Arguments(completer = classOf[FileDirCompleter])
  var arguments: java.util.List[String] = null
  
  @Override
  override def execute(commandInvocation: CommandInvocation): CommandResult = {
    val argsOpt = Option(arguments.asScala)

    if (help) {
      println(commandInvocation.getHelpInfo("cd"));
      CommandResult.SUCCESS
    } else {
      argsOpt match {
        case Some(pathLst) => 
          if (pathLst.length != 1) {
            println("cd: takes only 1 argument")
            CommandResult.FAILURE
          } else {
            val myDirOpt = verifyDirectory(pathLst.head)

            myDirOpt match {
              case Some((myDir, lastMyDirOpt, dirList)) =>
                val dirListStr = StringBuilder.newBuilder

                lastMyDirOpt match {
                  case Some(lastMyDir) =>
                    /* If dirList contains rootMyDir or curMyDir, rootMyDir/curMyDir must be the head
                     * So iterate on the list, dropping the head
                     * (because myDirStack doesn't ever contain rootMyDir, and it would already have curMyDir)
                     * Push each MyDir onto MyDirStack and append each name onto StringBuilder
                     * After iteration, push the last mile directory onto the stack
                     * And append the last mile directory's name onto StringBuilder
                     */
                    if (dirList.exists(_.eq(rootMyDir)) || dirList.exists(_.eq(curMyDir))) {
                      dirList.drop(1).foreach(dir => {
                        myDirStack.push(dir)
                        dirListStr.append(dir.getName)
                        dirListStr.append("/")
                      })
                      myDirStack.push(lastMyDir)
                      dirListStr.append(lastMyDir.getName)
                      dirListStr.append("/")
                    }
                    myDirStack.push(myDir)
                    dirListStr.append(myDir.getName)

                    // If we're changing directory from the current directory,
                    // use the current Prompt as a base for the new Prompt
                    // Otherwise just use the built dirListStr
                    val newPromptDirStr =
                      if (pathLst.head(0) != '/') commandInvocation.getPrompt.getPromptAsString.dropRight(2) + dirListStr.toString() + "/> "
                      else "/" + dirListStr.toString() + "/> "

                    commandInvocation.setPrompt(new Prompt(newPromptDirStr))
                  case None =>
                    if (myDir.eq(rootMyDir)) {
                      commandInvocation.setPrompt(new Prompt("/> "))
                    } else {
                      // If dir isn't the root dir then this has to be from just ..s
                      // Reverse the stack and iterate to get new prompt
                      val dirList = myDirStack.getBackedList.reverse
                      dirList.foreach(dir => {
                        dirListStr.append(dir.getName)
                        dirListStr.append("/")
                      })

                      val newPromptDirStr = "/" + dirListStr.toString() + "> "
                      commandInvocation.setPrompt(new Prompt(newPromptDirStr))
                    }
                }

                // Update curMyDir since we changed directory
                curMyDir = myDir

                CommandResult.SUCCESS
              case None =>
                CommandResult.FAILURE
            }
          }
       case None => 
          println("cd: requires an argument")
          CommandResult.FAILURE
      }
    }
  }

}

