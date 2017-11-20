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
//import util.Try


object MyDrive {
  private val APPLICATION_NAME = "scala-gdrive-project"
  private val DATA_STORE_DIR = System.getProperty("user.home") + "/.credentials/scala-gdrive-project"
  private val DATA_STORE_FACTORY =
    new FileDataStoreFactory(new java.io.File(DATA_STORE_DIR))
  private val JSON_FACTORY = JacksonFactory.getDefaultInstance
  private val HTTP_TRANSPORT = GoogleNetHttpTransport.newTrustedTransport
  private val SCOPES = ListBuffer[String](DriveScopes.DRIVE).asJava

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
      HTTP_TRANSPORT, JSON_FACTORY, authorize)
      .setApplicationName(APPLICATION_NAME)
      .build()
  }

  def getDirectoryStructure(service: com.google.api.services.drive.Drive): MyDir = {
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

    println("Done processing information")

    val files = totalList.toList

    val dirIdToGFileMap = scala.collection.mutable.Map[String, GFile]()
    val dirIdToDirsMap = scala.collection.mutable.Map[String, ListBuffer[String]]()

    // Add each folder to map such that (Directory ID -> GFile)
    files.filter(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {dirIdToGFileMap += (file.getId -> file)})

    // Add each folder (directory ID) to the list of directory IDs for its parent
    files.filter(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {
      val parentsOpt = Option(file.getParents.asScala)

      parentsOpt match {
        //case Some(parents) => println(s"mimeType: $mimeType, parents: $parents")
        case Some(parents) => {
          val childDirId = file.getId

          parents.foreach(parent => {
            if (!dirIdToDirsMap.contains(parent)) {
              dirIdToDirsMap += (parent -> new ListBuffer[String]())
            }

            val dirs = dirIdToDirsMap(parent)
            dirs += childDirId
          })
        }
        case None => ()
      }
    })

    val dirIdToGFilesListMap = scala.collection.mutable.Map[String, ListBuffer[GFile]]()

    files.filterNot(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {
      val parentsOpt = Option(file.getParents.asScala)

      parentsOpt match {
        case Some(parents) => {
          parents.foreach(parent => {
            if (!dirIdToGFilesListMap.contains(parent)) {
              dirIdToGFilesListMap += (parent -> new ListBuffer[GFile]())
            }

            val dirs = dirIdToGFilesListMap(parent)
            dirs += file
          })
        }
        case None => ()
      }
    })

    val dirIdQueue = Queue[String]()
    val rootDirId = service.files().get("root").execute().getId
    val rootChildDirIds = dirIdToDirsMap(rootDirId)
    val rootDir: MyDir = new MyDir("root", rootDirId, dirIdToGFilesListMap(rootDirId).toList)
    val dirIdToMyDir = scala.collection.mutable.Map[String, MyDir]()

    dirIdToMyDir += rootDirId -> rootDir

    Option(rootChildDirIds).foreach(x => {dirIdQueue ++= x})

    // This is based on assumption that the directory structure follows a tree-like structure
    // This probably won't work for loops
    while (!dirIdQueue.isEmpty) {
      val dirId = dirIdQueue.dequeue()
      val gfile = dirIdToGFileMap(dirId)

      // Create new MyDir, add it to map, add it parent MyDir's childDirs
      val newMyDir = new MyDir(gfile.getName, gfile.getId, util.Try(dirIdToGFilesListMap(dirId).toList).getOrElse(null))
      dirIdToMyDir += dirId -> newMyDir

      // Add children to queue
      util.Try(dirIdToDirsMap(dirId)) match {
        case util.Success(lstBuf) => dirIdQueue ++= lstBuf
        case util.Failure(_) => ()
      }

    }

    dirIdToMyDir.keys.filterNot(_.equals(rootDirId)).foreach(dirId => {
      Option(dirIdToGFileMap(dirId).getParents).foreach(_.forEach(parent => {dirIdToMyDir(parent).childDirs += dirIdToMyDir(dirId)}))
    })

    rootDir
  }

  def main(args: Array[String]): Unit = {
    val service = getDriveService

    val rootMyDir = getDirectoryStructure(service)

    println(rootMyDir)

    /*
    files.forEach(file => {
      val parentsOpt = Option(file.getParents.asScala)
      val mimeType = file.getMimeType
      //if (mimeType == "application/vnd.google-apps.folder") {
        parentsOpt match {
          case Some(parents) => println(s"mimeType: $mimeType, parents: $parents")
          case None => ()
        }
      //}
    })
    */

    /*
    */

    //println(files.get(0).getClass)
    //println("Files: ")
    /*
    files.forEach(file => {
      val name = file.getName
      val id = file.getId
      println(s"$name ($id)\n")
    })
    */

    /*
    val filenameList = ListBuffer[String]()

    totalList.foreach(file => {
      filenameList += file.getName
    })

    val fivesList = filenameList.sorted.grouped(5)

    fivesList.foreach(println(_))
    */
  }
}
