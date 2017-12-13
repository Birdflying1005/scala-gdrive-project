import java.io._
import java.nio.charset.{Charset, StandardCharsets}

import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => GFile}
import com.google.gson.{Gson, GsonBuilder}
import com.google.gson.reflect.TypeToken

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import java.util.concurrent.Executors

import AeshExample.ExampleValidatorInvocationProvider
import com.diogonunes.jcdp.color.ColoredPrinter
import com.diogonunes.jcdp.color.api.Ansi.{Attribute, BColor, FColor}
import com.google.api.client.util.DateTime
import me.tongfei.progressbar.ProgressBar
import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation
import org.jboss.aesh.console.{AeshConsoleBuilder, Prompt}
import org.jboss.aesh.console.settings.SettingsBuilder
import org.jboss.aesh.cl.completer.OptionCompleter
import org.jboss.aesh.console.command.completer.CompleterInvocation

import scala.io.Source
import scala.annotation.tailrec

import MyDriveService._

object MyDrive {
  private val service: Drive = getDriveService
  private var totalFileList: ListBuffer[GFile] = populateTotalFileList(service)
  private var rootDirGFile: GFile = service.files().get("root").execute()
  private var rootMyDir: MyDir = getDirectoryStructure(totalFileList, rootDirGFile)
  private var curMyDir: MyDir = rootMyDir

  private val colorPrinter = new ColoredPrinter.Builder(1, false).build()

  private val myDirStack = new MyDirStack()

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

      verifyPath(rootMyDir, curMyDir, myDirStack, path) match {
        case Right(lastTuple) => {
          val lastDirName = lastTuple._1
          val lastMyDir = lastTuple._2

          val possibilities: List[String] = lastMyDir.childDirs.map(_.name).filter(_.startsWith(lastDirName)).toList :::
            lastMyDir.childFiles.map(_.getName).filter(_.startsWith(lastDirName)).toList

          completerInvocation.addAllCompleterValues(possibilities.asJavaCollection)
        }
        case Left(failure) => {
          failure match {
            case InvalidPathFailure(_) => ()
            case DuplicateNameFailure(_) => ()
            case DoubleDotPlacementFailure => ()
            case DoubleDotCountFailure => ()
            case EmptyFollowingPathFailure(dir) => {
              val possibilities: List[String] = dir.childDirs.map(_.name).toList :::
                dir.childFiles.map(_.getName).toList

              completerInvocation.addAllCompleterValues(possibilities.asJavaCollection)
            }
          }
        }
      }
    }
  }

  @CommandDefinition(name="exit", description = "exit the program")
  object ExitCommand extends Command[CommandInvocation] {
    @Override
    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      commandInvocation.stop()
      CommandResult.SUCCESS
    }
  }

  // Dummy Command
  @CommandDefinition(name="foo", description = "fooing")
  object FooCommand extends Command[CommandInvocation] {

    @cl.Option(shortName = 'b', hasValue = false, description = "set boo to true/false")
    var bar: String = "bar"
    @cl.Option(shortName = 'f', hasValue = false, description = "set foo to true/false")
    var foo: String = "foo"

    @Arguments()
    val arguments: java.util.List[String] = null

    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      val out = commandInvocation.getShell.out()

      if (bar == null) {
        println("NO BAR!")
        val width = commandInvocation.getShell.getSize.getWidth
        val entriesMaxLength = curMyDir.lsEntries.map(_.gfile.getName.length).max
        val entriesPerLine = Math.floor(commandInvocation.getShell.getSize.getWidth.toDouble / (entriesMaxLength+2).toDouble).toInt
        println(s"width: $width, entriesMaxLength: $entriesMaxLength, entriesPerLine: $entriesPerLine")
      }
      else {
        println("You set bar to: " + bar)
        println("Let's work a bit......")
        Thread.sleep(2000)
      }

      CommandResult.SUCCESS
    }
  }

  @CommandDefinition(name="forceUpdate", description = "force update of state")
  object ForceUpdateCommand extends Command[CommandInvocation] {
    @cl.Option(shortName = 'h', hasValue = false, description = "display this help and exit")
    private var help: Boolean = false

    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      if (help) {
        println(commandInvocation.getHelpInfo("forceUpdate"));
        CommandResult.FAILURE
      } else {
        totalFileList = populateTotalFileList(service)
        rootDirGFile = service.files().get("root").execute()
        rootMyDir = getDirectoryStructure(totalFileList, rootDirGFile)
        curMyDir = rootMyDir

        commandInvocation.setPrompt(new Prompt("/> "))
        CommandResult.SUCCESS
      }
    }
  }

  object NameOrdering extends Ordering[LsEntry] {
    override def compare(x: LsEntry, y: LsEntry): Int = x.name.compareTo(y.name)
  }

  object SizeOrdering extends Ordering[LsEntry] {
    override def compare(x: LsEntry, y: LsEntry): Int = x.gfile.getSize.compareTo(y.gfile.getSize)
  }

  // TODO: Fix time ordering
  object TimeOrdering extends Ordering[LsEntry] {
    override def compare(x: LsEntry, y: LsEntry): Int = {
      //x.gfile.getModifiedTime.value.compareTo(y.gfile.getModifiedTime.value)
      x.gfile.getModifiedTime.toString.compareTo(y.gfile.getModifiedTime.toString)
      /*
      val xTime = x.gfile.getModifiedTime.value
      val yTime = y.gfile.getModifiedTime.value
      xTime.compareTo(yTime)
      */
    }
  }

  object ExtensionOrdering extends Ordering[LsEntry] {
    override def compare(x: LsEntry, y: LsEntry): Int = {
      val xDotIdx = x.name.indexOf('.')
      val yDotIdx = y.name.indexOf('.')

      if (xDotIdx == -1) {
        // Both files don't have extensions, so compare them
        if (yDotIdx == -1) x.name.compareTo(y.name)
        // x doesn't have extension, but y does
        else -1
      } else {
        val xExt = x.name.slice(xDotIdx+1, x.name.length)
        val yExt = y.name.slice(yDotIdx+1, y.name.length)

        // x has extension, y doesn't, so -1
        if (yDotIdx == -1) 1
        else {
          val compareVal = xExt.compareTo(yExt)

          // Extensions are same so compare them
          if (compareVal == 0) x.name.compareTo(y.name)
          // Else just go with the extension comparison
          else compareVal
        }
      }
    }
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
      if (!myDir.lsEntries.isEmpty) {
        val maxEntryLength = {
          if (showId) {
            myDir.lsEntries.map(_.gfile.getName.length).max + myDir.lsEntries.map(_.gfile.getId.length).max + 3
          } else {
            myDir.lsEntries.map(_.gfile.getName.length).max
          }
        }

        val maxEntriesPerLine = Math.floor(commandInvocation.getShell.getSize.getWidth.toDouble / (maxEntryLength + 2).toDouble).toInt

        // Sort entries
        val sortedLsEntries = sortBy match {
          case "name" => myDir.lsEntries.sorted(NameOrdering)
          case "size" => myDir.lsEntries.sorted(SizeOrdering)
          case "time" => myDir.lsEntries.sorted(TimeOrdering)
          case "extension" => myDir.lsEntries.sorted(ExtensionOrdering)
        }

        if (maxEntriesPerLine < 2) {
          //

          /*
           * If the width too short or line length too long, just iterate over sortedLsEntries
           * Print in blue if directory, else just normal color
           * Add newline at end of each iteration
           */
          sortedLsEntries.foreach(entry => {
            val entryName = if (showId) entry.gfile.getName + " (" + entry.gfile.getId + ")" else entry.gfile.getName
            if (entry.isDir) {
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
              val entryName = if (showId) entry.gfile.getName + " (" + entry.gfile.getId + ")" else entry.gfile.getName
              val formattedEntryName = String.format(formatstr, entryName)
              if (entry.isDir) {
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

    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      val results = new ListBuffer[CommandResult]()
      val argOpts = Option(arguments.asScala)

      if (help) {
        println(commandInvocation.getHelpInfo("ls"));
        CommandResult.FAILURE
      } else {
        argOpts match {
          // If we have arguments
          case Some(pathLst) => {
            // For each path in the list of paths
            pathLst.foreach(path => {
              // Verify the path
              verifyPath(rootMyDir, curMyDir, myDirStack, path) match {
                case Right((lastDirFileName, lastMyDir, _, _)) => {
                  if (lastDirFileName.contains('*')) {
                    // Replace all with "*"s with ".*"s
                    // What the regex basically is looking for is specified wildcards
                    // TODO: Maybe replace with if statements
                    val regExpStr = "^" + lastDirFileName.replaceAll("\\*", ".*") + "$"
                    val regExp = regExpStr.r

                    // Since bash's ls separates them, I decided to separate them as well
                    val childDirsFound = lastMyDir.childDirs.filter(childDir => { regExp.findFirstIn(childDir.name) != None })
                    val childFilesFound = lastMyDir.childFiles.filter(childFile => { regExp.findFirstIn(childFile.getName) != None })

                    if (childDirsFound.isEmpty && childFilesFound.isEmpty) {
                      println("No such last mile directory or file " + lastDirFileName)
                      results += CommandResult.FAILURE
                    } else {
                      childFilesFound.foreach(childFile => {
                        val output = if (showId) childFile.getName + " (" + childFile.getId + ")" else childFile.getName
                        println(output)
                      })

                      childDirsFound.foreach(childDir => {
                        if (listDirectory) {
                          colorPrinter.print(s"${childDir.getName}", Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                          colorPrinter.clear()
                        } else {
                          colorPrinter.print(s"${childDir.getName}:", Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                          colorPrinter.clear()
                          println()
                          lsDirHelper(commandInvocation, childDir)
                        }
                        println()
                      })
                      println()
                      results += CommandResult.SUCCESS
                    }
                  } else {
                    // Look for any child dirs or files that match lastDirFileName
                    val childDirsFound = lastMyDir.childDirs.filter(_.name.equals(lastDirFileName))
                    val childFilesFound = lastMyDir.childFiles.filter(_.getName.equals(lastDirFileName))

                    if (childDirsFound.isEmpty && childFilesFound.isEmpty) {
                      println("No such last mile directory or file " + lastDirFileName)
                      results += CommandResult.FAILURE
                    } else if (childDirsFound.length > 1) {
                      println("Multiple matches found for last mile directory " + lastDirFileName +
                        ". Please cd to the directory before it and use cdId command.")
                      results += CommandResult.FAILURE
                    } else {
                      if (!childDirsFound.isEmpty) {
                        if (listDirectory) {
                          colorPrinter.print(s"$path", Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                          colorPrinter.clear()
                        } else {
                          colorPrinter.print(s"$path:", Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                          colorPrinter.clear()
                          println()
                          lsDirHelper(commandInvocation, childDirsFound.head)
                        }
                        println("\n")
                        results += CommandResult.SUCCESS
                      } else {
                        // We found a file so just print out the filename
                        val output = if (showId) childFilesFound.head.getName + " (" + childFilesFound.head.getId + ")" else childFilesFound.head.getName
                        println(output)
                        println("\n")
                        results += CommandResult.SUCCESS
                      }
                    }
                  }
                }
                case Left(failure) => {
                  failure match {
                    case InvalidPathFailure(dirName) => {
                      println("No such directory " + dirName)
                      results += CommandResult.FAILURE
                    }
                    case DuplicateNameFailure(dirName) => {
                      println("Multiple directories with name " + dirName)
                      results += CommandResult.FAILURE
                    }
                    case DoubleDotPlacementFailure => {
                      println("Only leading ..s are supported")
                      results += CommandResult.FAILURE
                    }
                    case DoubleDotCountFailure => {
                      println("Too many ..s")
                      results += CommandResult.FAILURE
                    }
                    case EmptyFollowingPathFailure(dir) => {
                      if (listDirectory) {
                        colorPrinter.print(s"$path", Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                        colorPrinter.clear()
                      } else {
                        colorPrinter.print(s"$path:", Attribute.BOLD, FColor.BLUE, BColor.BLACK)
                        colorPrinter.clear()
                        println()
                        lsDirHelper(commandInvocation, dir)
                      }
                      println("\n")
                      results += CommandResult.SUCCESS
                    }
                  }
                }
              }
            })
          }
          case None => {
            lsDirHelper(commandInvocation, curMyDir)
            results += CommandResult.SUCCESS
          }
        }

        if (results.contains(CommandResult.FAILURE)) {
          CommandResult.FAILURE
        } else {
          CommandResult.SUCCESS
        }
      }

    }
  }


  @CommandDefinition(name="cd", description="changes directory")
  object CdCommand extends Command[CommandInvocation] {
    @cl.Option(shortName = 'h', hasValue = false, description = "display this help and exit")
    private var help: Boolean = false

    @Arguments(completer = classOf[FileDirCompleter])
    var arguments: java.util.List[String] = new java.util.LinkedList[String]()

    @Override
    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      val argsOpt = Option(arguments.asScala)

      if (help) {
        println(commandInvocation.getHelpInfo("cd"));
        CommandResult.SUCCESS
      } else {
        argsOpt match {
          case Some(lst) => {
            if (lst.length != 1) {
              println("cd: takes 1 argument")
              CommandResult.FAILURE
            } else {
              // Make sure the path is correct via verifyPath
              verifyPath(rootMyDir, curMyDir, myDirStack, lst.head) match {
                case Right((lastDirName, lastMyDir, dirList, leadingSlash)) => {
                  val childDirsFound = lastMyDir.childDirs.filter(_.name.equals(lastDirName))

                  if (childDirsFound.isEmpty) {
                    // Return FAILURE if the last mile directory
                    // isn't found
                    println("No such last mile directory " + lastDirName)
                    CommandResult.FAILURE
                  } else {
                    if (childDirsFound.length > 1) {
                      // Return FAILURE if multiple directories of
                      // the same name
                      println("Multiple matches found for last mile directory " + lastDirName +
                        ". Please cd to the directory before it and use cdId command.")
                      CommandResult.FAILURE
                    } else {
                      val myDir = childDirsFound.head

                      val dirListStr = StringBuilder.newBuilder
                      // If there's a leading slash, clear stack,
                      // iterate through directory list, add each to stack,
                      // and add lastMyDir (assuming none are rootMyDir)
                      if (leadingSlash) {
                        myDirStack.clear

                        dirList.foreach(dir => {
                          if (!dir.eq(rootMyDir)) {
                            myDirStack.push(dir)
                            dirListStr.append(dir.getName)
                            dirListStr.append("/")
                          }
                        })

                        if (!lastMyDir.eq(rootMyDir)) {
                          myDirStack.push(lastMyDir)
                          dirListStr.append(lastMyDir.getName)
                          dirListStr.append("/")
                        }

                        myDirStack.push(myDir)
                      // If no leading slash (aka cd from current dir)
                      } else {
                        // Determine if the directory list has curMyDir in it
                        // If so, slice dirList after it, iterate and add
                        // each MyDir to stack, then add lastMyDir to stack
                        if (dirList.exists(_.eq(curMyDir))) {
                          val firstOccurIdx = dirList.indexOf(curMyDir)
                          val slicedLst = dirList.slice(firstOccurIdx+1, dirList.length)

                          slicedLst.foreach(dir => {
                            myDirStack.push(dir)
                            dirListStr.append(dir.getName)
                            dirListStr.append("/")
                          })
                          myDirStack.push(lastMyDir)
                          dirListStr.append(lastMyDir.getName)
                          dirListStr.append("/")
                        }
                        // If the directory list doesn't have curMyDir in it
                        // That means dirList is empty and lastMyDir is curMyDir
                        // so do nothing

                        // Either way push myDir which is the last mile directory to cd to
                        myDirStack.push(myDir)
                      }

                      // Get prompt, drop the "> " from it
                      val curPromptDirStr = commandInvocation.getPrompt.getPromptAsString.dropRight(2)
                      // If we're changing directory from the current directory,
                      // use the current Prompt as a base for the new Prompt
                      // Otherwise just use the built dirListStr
                      val newPromptDirStr =
                        if (lst.head(0) != '/') curPromptDirStr + dirListStr.toString() + lastDirName + "/> "
                        else "/" + dirListStr.toString() + lastDirName + "/> "

                      commandInvocation.setPrompt(new Prompt(newPromptDirStr))

                      // Update curMyDir since we changed directory
                      curMyDir = myDir

                      CommandResult.SUCCESS
                    }
                  }
                }
                case Left(failure) => {
                  failure match {
                    case InvalidPathFailure(dirName) => {
                      println("No such directory " + dirName)
                      CommandResult.FAILURE
                    }
                    case DuplicateNameFailure(dirName) => {
                      println("Multiple directories with name " + dirName)
                      CommandResult.FAILURE
                    }
                    case DoubleDotPlacementFailure => {
                      println("Only leading ..s are supported")
                      CommandResult.FAILURE
                    }
                    case DoubleDotCountFailure => {
                      println("Too many ..s")
                      CommandResult.FAILURE
                    }
                    case EmptyFollowingPathFailure(dir) => {
                      if (dir.eq(rootMyDir)) {
                        commandInvocation.setPrompt(new Prompt("/> "))
                        curMyDir = rootMyDir
                      } else {
                        // If dir isn't the root dir then this has to be from just ..s
                        // Reverse the stack and iterate to get new prompt
                        val dirListStr = StringBuilder.newBuilder
                        val dirList = myDirStack.getBackedList.reverse
                        dirList.foreach(dir => {
                          dirListStr.append(dir.getName)
                          dirListStr.append("/")
                        })

                        val newPromptDirStr = "/" + dirListStr.toString() + "> "

                        commandInvocation.setPrompt(new Prompt(newPromptDirStr))
                        curMyDir = dir
                      }
                      CommandResult.SUCCESS
                    }
                  }
                }
              }
            }
          }
          case None => {
            println("cd: requires an argument")
            CommandResult.FAILURE
          }
        }
      }
    }
  }

  @CommandDefinition(name="cp", description="copies files to directories")
  object CpCommand extends Command[CommandInvocation] {
    @Arguments(completer = classOf[FileDirCompleter])
    var arguments: java.util.List[String] = new java.util.LinkedList[String]()

    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      val argsOpt = Option(arguments.asScala)

      argsOpt match {
        case Some(origLst) => {
          /*
           * So the original list will be file1 file2 ... filen directory
           * The easiest way to split the files and the directory is to
           * reverse the list
           */
          val revLst = origLst.reverse
          val copyToDir = revLst.head
          val filesToCopy = revLst.tail

          // Verify the directory to copy to exists
          val copyToDirId: Option[String] =
            verifyPath(rootMyDir, curMyDir, myDirStack, copyToDir) match {
              case Right((lastDirName, lastMyDir, _, _)) => {
                val dirList = lastMyDir.childDirs.filter(_.name == lastDirName)
                if (dirList.length == 1) {
                  Some(dirList.head.id)
                } else {
                  None
                }
              }
              case Left(failure) => {
                failure match {
                  case EmptyFollowingPathFailure(dir) => {
                    if (dir.eq(rootMyDir)) {
                      Some(rootMyDir.id)
                    } else {
                      Some(curMyDir.id)
                    }
                  }
                  case _ => None
                }
              }
            }

          // Verify the files to copy to exist
          val fileIds = ListBuffer[Option[String]]()
          filesToCopy.foreach(filepath => {
            val fileId = verifyPath(rootMyDir, curMyDir, myDirStack, filepath) match {
              case Right((lastFileName, lastMyDir, _, _)) => {
                val fileList = lastMyDir.childFiles.filter(_.getName == lastFileName)

                if (fileList.length == 1) {
                  Some(fileList.head.getId)
                } else {
                  None
                }
              }
              case Left(_) => None
            }

            fileIds.append(fileId)
          })

          if (copyToDirId == None || fileIds.contains(None)) {
            println("cp: Invalid directory or file(s) specified")
            CommandResult.FAILURE
          } else {
            // Second foreach to get rid of Option
            fileIds.foreach(_.foreach(fileId => {
              // Copying a file to a directory is just adding directory to file's parents
              service.files.update(fileId, new GFile()).setAddParents(copyToDirId.get).execute()
            }))
            CommandResult.SUCCESS
          }
        }
        case None => {
          println("cp: requires at least 2 arguments")
          CommandResult.FAILURE
        }
      }
    }
  }

  /** Writes state out to file in JSON format
    */
  def writeJSON: Unit = {
    val gson = new GsonBuilder().registerTypeHierarchyAdapter(classOf[ListBuffer[_]], new GsonListBufferAdapter()).create()

    val gfileJListType = new TypeToken[java.util.List[GFile]](){}.getType
    val totalFileListJson = gson.toJson(totalFileList.asJava, gfileJListType)

    val gfileType = new TypeToken[GFile](){}.getType
    val rootDirGFileJson = gson.toJson(rootDirGFile, gfileType)

    val myDirType = new TypeToken[MyDir](){}.getType
    val rootMyDirJson = gson.toJson(rootMyDir, myDirType)

    val f1 = new File("totalfilelist.txt")
    val pw1 = new PrintWriter(f1)
    pw1.write(totalFileListJson)
    pw1.close

    val f2 = new File("rootdirgfile.txt")
    val pw2 = new PrintWriter(f2)
    pw2.write(rootDirGFileJson)
    pw2.close

    val f3 = new File("rootmydir.txt")
    val pw3 = new PrintWriter(f3)
    pw3.write(rootMyDirJson)
    pw3.close()
  }

  def main(args: Array[String]): Unit = {

    val settings = new SettingsBuilder().logging(true).create()

    val consoleBuilder = new AeshConsoleBuilder().settings(settings)
                    .prompt(new Prompt("/> "))
      .addCommand(ExitCommand)
      .addCommand(LsCommand)
      .addCommand(FooCommand)
      .addCommand(CdCommand)
      .addCommand(CpCommand)
      .validatorInvocationProvider(new ExampleValidatorInvocationProvider())

    val console = consoleBuilder.create()

    console.start()
  }
}


