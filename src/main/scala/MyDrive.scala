import java.io._
import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer, Queue}
import scala.io.Source
import scala.util.{Try, Success, Failure}

import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => GFile}
import com.google.api.client.util.DateTime
import com.google.api.client.http.HttpHeaders
import com.google.api.client.googleapis.json.GoogleJsonError
import com.google.api.client.googleapis.batch.json.JsonBatchCallback
import com.google.gson.{Gson, GsonBuilder}
import com.google.gson.reflect.TypeToken

import com.diogonunes.jcdp.color.ColoredPrinter
import com.diogonunes.jcdp.color.api.Ansi.{Attribute, BColor, FColor}

import org.jboss.aesh.cl
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation
import org.jboss.aesh.console.{AeshConsoleBuilder, Prompt}
import org.jboss.aesh.console.settings.SettingsBuilder
import org.jboss.aesh.cl.completer.OptionCompleter
import org.jboss.aesh.console.command.completer.CompleterInvocation

import me.tongfei.progressbar.ProgressBar

import AeshExample.ExampleValidatorInvocationProvider

import MyDriveService._

object MyDrive {
  private val service: Drive = getDriveService
  private var totalFileList: ListBuffer[GFile] = populateTotalFileList(service)
  private var rootDirGFile: GFile = service.files().get("root").execute()
  private var directoryStructureTuple: (MyDir, scala.collection.mutable.Map[String, MyDir]) = getDirectoryStructure(totalFileList, rootDirGFile)
  private var rootMyDir: MyDir = directoryStructureTuple._1
  private var myDirById: scala.collection.mutable.Map[String, MyDir] = directoryStructureTuple._2
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

  @CommandDefinition(name="exit", description = "exit the program")
  object ExitCommand extends Command[CommandInvocation] {
    @Override
    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      commandInvocation.stop()
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
        getDirectoryStructure(totalFileList, rootDirGFile) match {
          case (myDir, map) =>
            directoryStructureTuple = null
            rootMyDir = myDir
            myDirById = map
        }
        curMyDir = rootMyDir

        commandInvocation.setPrompt(new Prompt("/> "))
        CommandResult.SUCCESS
      }
    }
  }

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
            val verifiedLst = verifyList(rootMyDir, curMyDir, myDirStack, pathLst.toList, true)
            val combinedLst = pathLst zip verifiedLst
              
            combinedLst.foreach({ case (path, elem) => {
              elem.get match {
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
              println()
            }})

            if (verifiedLst.exists(_.isEmpty)) CommandResult.FAILURE else CommandResult.SUCCESS
         case None =>
            lsDirHelper(commandInvocation, curMyDir)
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
              val myDirOpt = verifyDirectory(rootMyDir, curMyDir, myDirStack, pathLst.head)

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
            val verifiedLst = verifyList(rootMyDir, curMyDir, myDirStack, pathLst.toList, false) 
    
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
            val verifiedLst = verifyList(rootMyDir, curMyDir, myDirStack, pathLst.toList, true) 
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
              verifyPath(rootMyDir, curMyDir, myDirStack, path) match {
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

  /** 
   *  Writes state out to file in JSON format
   */
  def writeJSON(): Unit = {
    val gson = new GsonBuilder().registerTypeHierarchyAdapter(classOf[ListBuffer[_]], new GsonListBufferAdapter()).create()

    val gfileJListType = new TypeToken[java.util.List[GFile]](){}.getType
    val totalFileListJson = gson.toJson(totalFileList.asJava, gfileJListType)

    val gfileType = new TypeToken[GFile](){}.getType
    val rootDirGFileJson = gson.toJson(rootDirGFile, gfileType)

    val myDirType = new TypeToken[MyDir](){}.getType
    val rootMyDirJson = gson.toJson(rootMyDir, myDirType)

    val f1 = new File("/tmp/totalfilelist.txt")
    val pw1 = new PrintWriter(f1)
    pw1.write(totalFileListJson)
    pw1.close

    val f2 = new File("/tmp/rootdirgfile.txt")
    val pw2 = new PrintWriter(f2)
    pw2.write(rootDirGFileJson)
    pw2.close

    val f3 = new File("/tmp/rootmydir.txt")
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
      .addCommand(ForceUpdateCommand)
      .addCommand(CdCommand)
      .addCommand(RmCommand)
      .addCommand(MvCommand)
      .addCommand(MkdirCommand)
      .validatorInvocationProvider(new ExampleValidatorInvocationProvider())

    val console = consoleBuilder.create()

    console.start()
  }
}


