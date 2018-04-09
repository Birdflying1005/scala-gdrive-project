
package Drive.Commands

import Drive.Util.FileDirCompleter
import Drive.Util.MyDir
import Drive.MyDriveService._
import Drive.MyDriveState.colorPrinter;
import Drive.MyDriveState.curMyDir
import scala.collection.JavaConverters._
import com.diogonunes.jcdp.color.ColoredPrinter
import com.diogonunes.jcdp.color.api.Ansi.{Attribute, BColor, FColor}
import com.google.api.services.drive.model.{File => GFile}
import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation
import com.diogonunes.jcdp.color.ColoredPrinter
import com.diogonunes.jcdp.color.api.Ansi.{Attribute, BColor, FColor}
import scala.collection.JavaConverters._


object NameOrdering extends Ordering[GFile] {
  override def compare(x: GFile, y: GFile): Int = x.getName.compareTo(y.getName)
}

object SizeOrdering extends Ordering[GFile] {
  override def compare(x: GFile, y: GFile): Int = x.getSize.compareTo(y.getSize)
}

object TimeOrdering extends Ordering[GFile] {
  override def compare(x: GFile, y: GFile): Int = x.getModifiedTime.getValue.compareTo(y.getModifiedTime.getValue)
}

object ExtensionOrdering extends Ordering[GFile] {
  override def compare(x: GFile, y: GFile): Int = x.getFullFileExtension.compareTo(y.getFullFileExtension)
}



@CommandDefinition(name="ls", description = "list dirs")
object LsCommand extends Command[CommandInvocation] {
  @cl.Option(shortName = 's', name = "sort", description = "sort by", defaultValue = Array("name"))
  private var sortBy: String = ""

  @cl.Option(shortName = 'i', name = "show-id", description = "show ids", hasValue = false)
  private var showId: Boolean = false

  @cl.Option(shortName = 'h', hasValue = false, description = "display this help and exit")
  private var help: Boolean = false

  @cl.Option(shortName = 'd', hasValue = false, description = "list directories themselves, not their contents")
  private var listDirectory: Boolean = false

  @Arguments(completer = classOf[FileDirCompleter])
  private var arguments: java.util.List[String] = null

  /**
    * Prints out the listing for a certain directory
    *
    * @param commandInvocation
    * @param myDir certain directory
    */
  private def lsDirHelper(commandInvocation: CommandInvocation, myDir: MyDir): Unit = {
    // TODO: Perhaps create a ListBuffer[LsEntry] on-the-fly?
    // Currently prints dirs and files separately; looks weird
    def lsDirHelper2(doDirs: Boolean) = {
      val entries = if (doDirs) myDir.childDirs.map(_.gfile) else myDir.childFiles

      if (!entries.isEmpty) {
        val maxEntryLength = {
          if (showId) {
            entries.map(_.getName.length).max + entries.map(_.getId.length).max + 3
          } else {
            entries.map(_.getName.length).max
          }
        }

        val maxEntriesPerLine = Math.floor(commandInvocation.getShell.getSize.getWidth.toDouble / (maxEntryLength + 2).toDouble).toInt

        // Sort entries
        val sortedLsEntries = sortBy match {
          case "name" => entries.sorted(NameOrdering)
          case "size" => entries.sorted(SizeOrdering)
          case "time" => entries.sorted(TimeOrdering)
          case "extension" => entries.sorted(ExtensionOrdering)
        }

        if (maxEntriesPerLine < 2) {
          /*
           * If the width too short or line length too long, just iterate over sortedLsEntries
           * Print in blue if directory, else just normal color
           * Add newline at end of each iteration
           */
          sortedLsEntries.foreach(entry => {
            val entryName = if (showId) entry.getName + " (" + entry.getId + ")" else entry.getName
            if (doDirs) {
              colorPrinter.print(entryName, Attribute.BOLD, FColor.BLUE, BColor.BLACK)
              colorPrinter.clear()
            } else {
              print(entryName)
            }
            println()
          })
        } else {
          /*
           * Else if width not too short and line length not too long
           * Group list of entries into sublists based on maxEntryPerLine
           * Iterate over sublists
           * Format the string for entry based on maxEntryLength (aka pad with spaces)
           * Print in blue if directory, else just normal color
           * Add newline at end of sublist iteration
           */
          val groupedEntries = sortedLsEntries.grouped(maxEntriesPerLine)
          groupedEntries.foreach(group => {
            group.foreach(entry => {
              val formatstr = "%-" + (maxEntryLength + 2) + "s"
              val entryName = if (showId) entry.getName + " (" + entry.getId + ")" else entry.getName
              val formattedEntryName = String.format(formatstr, entryName)
              if (doDirs) {
                colorPrinter.print(formattedEntryName, Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                colorPrinter.clear()
              } else {
                print(formattedEntryName)
              }
            })
            println()
          })
        }
      }
    }

    lsDirHelper2(true)
    lsDirHelper2(false)
  }

  override def execute(commandInvocation: CommandInvocation): CommandResult = {
    val argOpts = Option(arguments.asScala)

    if (help) {
      println(commandInvocation.getHelpInfo("ls"));
      CommandResult.FAILURE
    } else {
      argOpts match {
        case Some(pathLst) => 
          val combinedLst = verifyList(pathLst.toList, true)
          //val combinedLst = pathLst zip verifiedLst
            
          combinedLst.foreach({ case (elem, path) => {
            elem match {
              case Some(verifiedElem) =>
                verifiedElem match {
                  case Left(gfile) =>
                    val output = if (showId) gfile.getName + " (" + gfile.getId + ")" else gfile.getName
                    println(output)
                  case Right(myDir) =>
                    if (listDirectory) {
                      colorPrinter.print(s"$path", Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                      colorPrinter.clear()
                    } else {
                      colorPrinter.print(s"$path:", Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                      colorPrinter.clear()
                      println()
                      lsDirHelper(commandInvocation, myDir)
                    }
                }
              case None => ()
            }
            println()
          }})

          if (combinedLst.map(x => x._1).exists(_.isEmpty)) CommandResult.FAILURE else CommandResult.SUCCESS
       case None =>
          lsDirHelper(commandInvocation, curMyDir)
          CommandResult.SUCCESS
      }
    }

  }
}


