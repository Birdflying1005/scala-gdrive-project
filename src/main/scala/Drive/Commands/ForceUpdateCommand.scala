package Drive.Commands

import Drive.MyDriveState
import Drive.MyDriveService._
import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation
import org.jboss.aesh.console.Prompt
import scala.collection.JavaConverters._

@CommandDefinition(name="forceUpdate", description = "force update of state")
object ForceUpdateCommand extends Command[CommandInvocation] {
  @cl.Option(shortName = 'h', hasValue = false, description = "display this help and exit")
  private var help: Boolean = false

  override def execute(commandInvocation: CommandInvocation): CommandResult = {
    if (help) {
      println(commandInvocation.getHelpInfo("forceUpdate"));
      CommandResult.FAILURE
    } else {
      MyDriveState.totalFileList = populateTotalFileList(MyDriveState.service)
      MyDriveState.rootDirGFile = MyDriveState.service.files().get("root").execute()
      getDirectoryStructure(MyDriveState.totalFileList, MyDriveState.rootDirGFile) match {
        case (myDir, map) =>
          MyDriveState.directoryStructureTuple = null
          MyDriveState.rootMyDir = myDir
          MyDriveState.myDirById = map
      }
      MyDriveState.curMyDir = MyDriveState.rootMyDir

      commandInvocation.setPrompt(new Prompt("/> "))
      CommandResult.SUCCESS
    }
  }
}
