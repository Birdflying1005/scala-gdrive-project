package Drive

import java.io._
import java.util.concurrent.Executors
import scala.concurrent._
            

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer, Queue}
import scala.io.Source
import scala.util.{Try, Success, Failure}

import com.google.api.services.drive.model.{File => GFile}
import com.google.api.client.util.DateTime
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

import me.tongfei.progressbar.ProgressBar

import MyDriveService._
import MyDriveState._
import Drive.Util._
import Drive.GsonAdapters._
import Drive.Commands._


object MyDrive {
//  def executeHelper(help: Boolean, cmdName: String, commandInvocation: CommandInvocation, realEvecuteFunc: () => Unit) {
//    if (help) {
//      commandInvocation.getHelpInfo();
//    } else {
//      realExecuteFunc();
//    }
//  }

  /** 
   *  Writes state out to file in JSON format
   */
  def writeJSON(): Unit = {
    val gson = new GsonBuilder().registerTypeHierarchyAdapter(classOf[ListBuffer[_]], new GsonListBufferAdapter()).create()

    val gfileJListType = new TypeToken[java.util.List[GFile]](){}.getType
    val totalFileListJson = gson.toJson(MyDriveState.totalFileList.asJava, gfileJListType)

    val gfileType = new TypeToken[GFile](){}.getType
    val rootDirGFileJson = gson.toJson(MyDriveState.rootDirGFile, gfileType)

    val myDirType = new TypeToken[MyDir](){}.getType
    val rootMyDirJson = gson.toJson(MyDriveState.rootMyDir, myDirType)

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

    val console = consoleBuilder.create()

    if (!totalFileList.isEmpty) {
      console.start()
    }
  }
}


