import java.io.{BufferedWriter, ByteArrayOutputStream, FileOutputStream, FileWriter}
import java.nio.charset.{Charset, StandardCharsets}

import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.drive.{Drive, DriveScopes}
import com.google.api.services.drive.model.{File => GFile}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import util.{Failure, Success, Try}
import java.util.concurrent.Executors
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import AeshExample.ExampleValidatorInvocationProvider
import com.diogonunes.jcdp.color.ColoredPrinter
import com.diogonunes.jcdp.color.api.Ansi.{FColor, BColor, Attribute}
import com.google.api.client.http.{HttpRequest, HttpRequestInitializer}
import me.tongfei.progressbar.ProgressBar
import org.jboss.aesh.cl
import org.jboss.aesh.cl.validator.{OptionValidator, OptionValidatorException}
import org.jboss.aesh.cl.{Arguments, CommandDefinition}
import org.jboss.aesh.console.command.{Command, CommandResult}
import org.jboss.aesh.console.command.invocation.CommandInvocation
import org.jboss.aesh.console.command.validator.{ValidatorInvocation, ValidatorInvocationProvider}
import org.jboss.aesh.console.{AeshConsoleBuilder, AeshContext, Prompt}
import org.jboss.aesh.console.settings.SettingsBuilder
import org.jboss.aesh.cl.completer.OptionCompleter
import org.jboss.aesh.console.command.completer.CompleterInvocation
/*
import org.jboss.aesh.cl.internal.ProcessedOptionBuilder
import org.jboss.aesh.cl.builder.CommandBuilder
import org.jboss.aesh.cl.completer.OptionCompleter
import org.jboss.aesh.console.command.completer.CompleterInvocation
import org.jboss.aesh.console.command.registry.AeshCommandRegistryBuilder
*/

import scala.annotation.tailrec

object MyDrive {
  private val APPLICATION_NAME = "scala-gdrive-project"
  private val DATA_STORE_DIR = System.getProperty("user.home") + "/.credentials/scala-gdrive-project"
  private val DATA_STORE_FACTORY =
    new FileDataStoreFactory(new java.io.File(DATA_STORE_DIR))
  private val JSON_FACTORY = JacksonFactory.getDefaultInstance
  private val HTTP_TRANSPORT = GoogleNetHttpTransport.newTrustedTransport
  private val SCOPES = ListBuffer[String](DriveScopes.DRIVE).asJava

  private val curMyDir: AtomicReference[MyDir] = new AtomicReference[MyDir]()
  private val rootMyDir: AtomicReference[MyDir] = new AtomicReference[MyDir]()
  private val totalFileList: AtomicReference[List[GFile]] = new AtomicReference[List[GFile]]()
  private val rootDirGFile: AtomicReference[GFile] = new AtomicReference[GFile]()

  private val averageWordLength: AtomicInteger = new AtomicInteger()

  private val colorPrinter = new ColoredPrinter.Builder(1, false).build()

  def setHttpTimeout(requestInitializer: HttpRequestInitializer): HttpRequestInitializer = {
    new HttpRequestInitializer() {
      override def initialize(request: HttpRequest): Unit = {
        requestInitializer.initialize(request)
        request.setConnectTimeout(3 * 60000)
        request.setReadTimeout(3 * 60000)
      }
    }
  }

  def authorize: Credential = {
    val clientSecrets = GoogleClientSecrets.load(JSON_FACTORY,
      new java.io.InputStreamReader(new java.io.FileInputStream(DATA_STORE_DIR + "/client_id.json")))
    val flow =
      new GoogleAuthorizationCodeFlow.Builder(HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, SCOPES)
        .setDataStoreFactory(DATA_STORE_FACTORY)
        .setAccessType("offline")
        .build()
    new AuthorizationCodeInstalledApp(flow, new LocalServerReceiver).authorize("user")
  }

  def getDriveService: com.google.api.services.drive.Drive = {
    new Drive.Builder(
      HTTP_TRANSPORT, JSON_FACTORY, setHttpTimeout(authorize))
      .setApplicationName(APPLICATION_NAME)
      .build()
  }

  def populateTotalFileList(service: com.google.api.services.drive.Drive) = {
    val totalList = ListBuffer[GFile]()
    var pageToken = ""

    do {
      val result = service.files.list
        .setPageSize(1000)
        .setPageToken(pageToken)
        .setFields("nextPageToken, files(id, name, parents, mimeType)")
        .execute()
      totalList ++= result.getFiles.asScala
      pageToken = result.getNextPageToken
      println(pageToken)
    } while (pageToken != null)

    totalFileList.set(totalList.toList)
    rootDirGFile.set(service.files().get("root").execute())

    //println("Done processing information")
  }

  def getDirectoryStructure: MyDir = {
    /*

    val filenameList = ListBuffer[String]()

    totalList.foreach(file => {
      filenameList += file.getName + " (" + file.getId + ")"
    })

    val fivesList = filenameList.sorted.grouped(5)

    fivesList.foreach(println(_))
    */

    val files = totalFileList.get()
    val rootDirId = rootDirGFile.get().getId

    averageWordLength.set(files.map(_.getName.length).sum/files.length)

    val dirGFileById = scala.collection.mutable.Map[String, GFile]()
    val childDirIdsById = scala.collection.mutable.Map[String, ListBuffer[String]]()
    val childGFilesById = scala.collection.mutable.Map[String, ListBuffer[GFile]]()

    // Add each folder to map such that (Directory ID -> GFile)
    //files.filter(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {dirGFileById += (file.getId -> file)})
    dirGFileById += rootDirId -> rootDirGFile.get()

    // Add each folder (directory ID) to the list of folders (directory IDs) for its parent
    // Add each folder to the list of folders for its parent
    files.filter(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {
      dirGFileById += (file.getId -> file)
      Option(file.getParents).foreach(
        _.asScala.foreach(parent => {
          if (!childDirIdsById.contains(parent)) childDirIdsById += (parent -> new ListBuffer[String]())

          childDirIdsById(parent) += file.getId
        })
      )
    })

    files.filterNot(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {
      Option(file.getParents).foreach(
        _.asScala.foreach(parent => {
          if (!childGFilesById.contains(parent)) childGFilesById += (parent -> new ListBuffer[GFile]())

          childGFilesById(parent) += file
        })
      )
    })

    val dirIdQueue = Queue[String]()
    val myDirById = scala.collection.mutable.Map[String, MyDir]()

    val rootChildDirIds = childDirIdsById(rootDirId)
    val rootMyDir = new MyDir("/", rootDirId, childGFilesById(rootDirId).toList)

    myDirById += (rootDirId -> rootMyDir)
    Option(rootChildDirIds).foreach(x => {dirIdQueue ++= x})

    // This is based on assumption that the directory structure follows a tree-like structure
    // Although I am not 100% sure this is the case, AFAIK unix filesystems prevent this
    // https://unix.stackexchange.com/a/22406
    while (!dirIdQueue.isEmpty) {
      val dirId = dirIdQueue.dequeue()
      val gfile = dirGFileById(dirId)

      //println("dirId: " + dirId + ", name: " + gfile.getName)
      // Create new MyDir, add it to map, add it parent MyDir's childDirs
      val newMyDir = new MyDir(gfile.getName, gfile.getId, util.Try(childGFilesById(dirId).toList).getOrElse(List()))
      myDirById += dirId -> newMyDir

      // Add children to queue
      Try(childDirIdsById(dirId)).foreach(childDirIds => {dirIdQueue ++= childDirIds})
    }

    val dirIdsNotRoot = myDirById.keys.filterNot(_.equals(rootDirId))

    // Add each MyDir to its parents
    dirIdsNotRoot.foreach(dirId => {
      Option(dirGFileById(dirId).getParents)
        .foreach(_.asScala.foreach(parent => {
          //println("parent: " + parent + ", child: " + dirId)
          myDirById(parent).childDirs += myDirById(dirId)
        }))
    })

    rootMyDir
  }

  def getCurDir: MyDir = curMyDir.get()

  def getRootDir: MyDir = rootMyDir.get()

  def verifyPath(path: String): util.Try[(String, MyDir, List[String])] = {
    val tuple = path(0) match {
      case '/' => (MyDrive.getRootDir, path.slice(1, path.length).split("/").toList)
      case _ => (MyDrive.getCurDir, path.split("/").toList)
    }

    @tailrec
    def verifyDirs(curDir: MyDir, theLst: List[String], acc: ListBuffer[String]): (String, MyDir, List[String]) = {
      theLst match {
        case hd :: tl => {
          if (tl.isEmpty) {
            (hd.toString, curDir, acc.toList)
          } else {
            val matchingDirs = curDir.childDirs.filter(_.name == hd)

            if (matchingDirs.length == 0) {
              throw new Exception()
            } else if (matchingDirs.length == 1) {
              verifyDirs(matchingDirs.head, tl, acc += curDir.name)
            } else {
              // TODO: Handle multiple directories with the same name
              throw new Exception()
            }
          }
        }
          // TODO: Handle case of '/'
        case Nil => {
          (null, null, null)
        }
      }
    }

    util.Try(verifyDirs(tuple._1, tuple._2, ListBuffer[String]()))
  }

  def write(out: java.io.OutputStream, string: String): Unit = {
    out.write(string.getBytes(StandardCharsets.UTF_8))
  }

  def writeln(out: java.io.OutputStream, string: String): Unit = {
    write(out, string + "\n")
  }


  class CdCompleter extends OptionCompleter[CompleterInvocation] {
    override def complete(completerInvocation: CompleterInvocation): Unit = {
      //val completeVal = Option(completerInvocation.getGivenCompleteValue())
      val compVal = completerInvocation.getGivenCompleteValue

      MyDrive.verifyPath(compVal) match {
        case util.Success(lastTuple) => {
          val lastDir = lastTuple._1
          val lastMyDir = lastTuple._2

          val possibilities: List[String] = lastMyDir.childDirs.map(_.name).filter(_.startsWith(lastDir)).toList :::
            lastMyDir.childFiles.map(_.getName).filter(_.startsWith(lastDir))

          completerInvocation.addAllCompleterValues(possibilities.asJavaCollection)
        }
        case util.Failure(_) => ()
      }
    }
  }

  class DirectoryPathValidator extends OptionValidator[DirectoryPathInvocation] {
    override def validate(validatorInvocation: DirectoryPathInvocation): Unit = {
      val tryRes = verifyPath(validatorInvocation.getValue)

      tryRes match {
        case Success(tuple) => {
          val lastDir = tuple._1
          val lastMyDir = tuple._2

          // TODO: Deal with multiple directories with same name
          val childDirsFound = lastMyDir.childDirs.map(_.name).filter(_.equals(lastDir))

          if (childDirsFound.length != 1)
            throw new OptionValidatorException("Directory path validation failed.")
        }
        case Failure(_) => throw new OptionValidatorException("Directory path validation failed.")
      }
    }
  }

  class DirectoryPathInvocation(val path: String, val command: Command[CommandInvocation], val aeshContext: AeshContext)
    extends ValidatorInvocation[String, Command[CommandInvocation]] {
    override def getCommand: Command[CommandInvocation] = command

    override def getValue: String = path

    override def getAeshContext: AeshContext = aeshContext
  }

  class ExampleValidatorInvocationProvider extends ValidatorInvocationProvider[ValidatorInvocation[String, Command[CommandInvocation]]] {
    override def enhanceValidatorInvocation(validatorInvocation: ValidatorInvocation[_, _]): ValidatorInvocation[String, Command[CommandInvocation]] = {
      if (validatorInvocation.getValue.isInstanceOf[String])
        new DirectoryPathInvocation(validatorInvocation.getValue.asInstanceOf[String],
          validatorInvocation.getCommand.asInstanceOf[Command[CommandInvocation]], validatorInvocation.getAeshContext)
      else
        validatorInvocation.asInstanceOf[ValidatorInvocation[String, Command[CommandInvocation]]]
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

  @CommandDefinition(name="foo", description = "fooing")
  object FooCommand extends Command[CommandInvocation] {

    @cl.Option(shortName = 'b', hasValue = false, description = "set boo to true/false")
    var bar: String = "bar"
    @cl.Option(shortName = 'f', hasValue = false, description = "set foo to true/false")
    var foo: String = "foo"

    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      val out = commandInvocation.getShell.out()

      if (bar == null)
        writeln(out, "NO BAR!")
      else {
        writeln(out, "You set bar to: " + bar)
        writeln(out, "Let's work a bit......")
        println("Test")
        println(commandInvocation.getShell.getSize.getWidth.toString)
        println(averageWordLength.get())
        Thread.sleep(2000)
      }

      CommandResult.SUCCESS
    }
  }

  @CommandDefinition(name="ls", description = "list dirs")
  object LsCommand extends Command[CommandInvocation] {
    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      val entriesPerLine = Math.floor(commandInvocation.getShell.getSize.getWidth.toDouble / averageWordLength.get().toDouble).toInt - 1
      /*
      println(commandInvocation.getShell.getSize.getWidth.toDouble)
      println(commandInvocation.getShell.getSize.getWidth.toDouble / averageWordLength.get().toDouble)
      println((commandInvocation.getShell.getSize.getWidth.toDouble / averageWordLength.get().toDouble).toInt)
      println((commandInvocation.getShell.getSize.getWidth.toDouble / averageWordLength.get().toDouble).toInt - 1)
      */

      // TODO: Format it so it looks better
      if (entriesPerLine < 2) {
        curMyDir.get().childDirs.map(_.name).foreach(entry => { colorPrinter.print(entry + "\n", Attribute.BOLD, FColor.BLUE, BColor.BLACK) })
        colorPrinter.clear()
        curMyDir.get().childFiles.map(_.getName).foreach(println(_))
      } else {
        curMyDir.get().childDirs.map(_.name).grouped(entriesPerLine).foreach(group => {
          group.foreach(entry => { colorPrinter.print(entry + "  ", Attribute.BOLD, FColor.BLUE, BColor.BLACK) })
          println()
        })

        colorPrinter.clear()

        curMyDir.get().childFiles.map(_.getName).grouped(entriesPerLine).foreach(group => {
          group.foreach(entry => { print(entry + "  ") })
        })
      }

      CommandResult.SUCCESS
    }
  }


  @CommandDefinition(name="cd", description="changes directory")
  object CdCommand extends Command[CommandInvocation] {
    //@Arguments(completer = classOf[CdCompleter], validator = classOf[DirectoryPathValidator])
    @Arguments(completer = classOf[CdCompleter])
    var arguments: java.util.List[String] = new java.util.LinkedList[String]()

    @Override
    override def execute(commandInvocation: CommandInvocation): CommandResult = {
      val argsOpt = Option(arguments.asScala)

      //argsOpt.map(_.foreach(println))

      argsOpt match {
        case Some(lst) => {
          if (lst.length != 1) {
            write(commandInvocation.getShell.out(), "This command does not take multiple arguments")
            CommandResult.FAILURE
          } else {
            // TODO: Add checking for .. with a synchronized Stack
            if (lst.head == "/") {
              curMyDir.set(rootMyDir.get())
              commandInvocation.setPrompt(new Prompt("/> "))

              CommandResult.SUCCESS
            } else {
              val tryRes = verifyPath(lst.head)

              tryRes match {
                case Success(tuple) => {
                  val lastDir = tuple._1
                  val lastMyDir = tuple._2
                  val dirList = tuple._3

                  // TODO: Deal with multiple directories with same name
                  val childDirsFound = lastMyDir.childDirs.filter(_.name.equals(lastDir))

                  val myDir = childDirsFound.head
                  curMyDir.set(myDir)

                  val dirListStr = StringBuilder.newBuilder

                  dirList.foreach(dir => {
                    dirListStr.append(dir)
                    dirListStr.append("/")
                  })

                  val curPromptDirStr = commandInvocation.getPrompt.getPromptAsString.dropRight(2)

                  val newPromptDirStr =
                    if (lst.head(0) != '/') curPromptDirStr + dirListStr.toString() + lastDir + "/> "
                    else dirListStr.toString() + lastDir + "/> "

                  commandInvocation.setPrompt(new Prompt(newPromptDirStr))
                  CommandResult.SUCCESS
                }
                case Failure(_) => {
                  write(commandInvocation.getShell.out(), "This command does not take multiple arguments")
                  CommandResult.FAILURE
                }
              }
            }
          }
        }
        case None => {
          CommandResult.FAILURE
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val service = getDriveService

    populateTotalFileList(service)
    rootMyDir.set(getDirectoryStructure)
    curMyDir.set(rootMyDir.get)

    //println(rootMyDir)

    val settings = new SettingsBuilder().logging(true).create()

    val consoleBuilder = new AeshConsoleBuilder().settings(settings)
                    .prompt(new Prompt("/> "))
      .addCommand(ExitCommand)
      .addCommand(LsCommand)
      .addCommand(FooCommand)
      .addCommand(CdCommand)
      .validatorInvocationProvider(new ExampleValidatorInvocationProvider())

    val console = consoleBuilder.create()

    console.start()
  }
}


