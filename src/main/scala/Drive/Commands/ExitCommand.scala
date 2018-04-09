
package Drive.Commands

import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation



@CommandDefinition(name="exit", description = "exit the program")
object ExitCommand extends Command[CommandInvocation] {
  @Override
  override def execute(commandInvocation: CommandInvocation): CommandResult = {
    commandInvocation.stop()
    CommandResult.SUCCESS
  }
}
