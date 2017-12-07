import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.drive.{Drive, DriveScopes}
import com.google.api.services.drive.model.{File => GFile}
import com.google.api.client.http.HttpRequestInitializer

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

object MyDriveService {
  private val APPLICATION_NAME = "scala-gdrive-project"
  private val DATA_STORE_DIR = System.getProperty("user.home") + "/.credentials/scala-gdrive-project"
  private val DATA_STORE_FACTORY =
    new FileDataStoreFactory(new java.io.File(DATA_STORE_DIR))
  private val JSON_FACTORY = JacksonFactory.getDefaultInstance
  private val HTTP_TRANSPORT = GoogleNetHttpTransport.newTrustedTransport
  private val SCOPES = List[String](DriveScopes.DRIVE).asJava
  private var PRINT = true

  /** Creates a new HttpRequestInitializer that takes an old HttpRequestInitializer
    * and extends the read and connect timeouts of every request
    *
    * @param requestInitializer old HttpRequestInitializer
    * @return a new HttpRequestInitializer with extended timeouts
    */
  def setHttpTimeout(requestInitializer: HttpRequestInitializer): HttpRequestInitializer = {
    // Single Abstract Method
    (request: com.google.api.client.http.HttpRequest) => {
      requestInitializer.initialize(request)
      request.setConnectTimeout(3 * 60000)
      request.setReadTimeout(3 * 60000)
    }
  }

  /** Authorizes application to use user data via OAuth2 and user's credentials
    *
    * @return a new Credential instance based on credentials in client_id.json
    */
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

  /** Builds an instance of Drive based on authorization credentials
    *
    * @return a new instance of Drive based on credentials
    */
  def getDriveService: com.google.api.services.drive.Drive = {
    new Drive.Builder(
      HTTP_TRANSPORT, JSON_FACTORY, setHttpTimeout(authorize))
      .setApplicationName(APPLICATION_NAME)
      .build()
  }

  def populateTotalFileList(service: com.google.api.services.drive.Drive) = {
    val totalList = ListBuffer[GFile]()
    var pageToken = ""

    if (PRINT) {
      print("Getting list of files...")
    }

    do {
      if (PRINT) {
        print(".")
      }

      val result = service.files.list
        .setPageSize(1000)
        .setPageToken(pageToken)
        .setFields("nextPageToken, files(id, name, parents, mimeType, modifiedTime, size)")
        .execute()
      totalList ++= result.getFiles.asScala
      pageToken = result.getNextPageToken
      //println(pageToken)
    } while (pageToken != null)

    if (PRINT) {
      println()
    }

    totalList
  }

  def getDirectoryStructure(totalFileList: ListBuffer[GFile], rootDirGFile: GFile): MyDir = {
    if (PRINT) {
      println("Creating directory structure...")
    }

    val files = totalFileList
    val rootDirId = rootDirGFile.getId

    val dirGFileById = scala.collection.mutable.Map[String, GFile]()
    val childDirIdsById = scala.collection.mutable.Map[String, ListBuffer[String]]()
    val childGFilesById = scala.collection.mutable.Map[String, ListBuffer[GFile]]()

    val lsEntryById = scala.collection.mutable.Map[String, LsEntry]()

    dirGFileById += rootDirId -> rootDirGFile

    // Add each directory to map such that (Directory ID -> GFile)
    // Add each directory to the list of directory for its parent
    files.filter(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {
      dirGFileById += (file.getId -> file)
      lsEntryById += file.getId -> new LsEntry(true, file.getName, file)
      Option(file.getParents).foreach(
        _.asScala.foreach(parent => {
          if (!childDirIdsById.contains(parent)) childDirIdsById += (parent -> new ListBuffer[String]())

          childDirIdsById(parent) += file.getId
        })
      )
    })

    // Add each file (file ID) to the list of files (GFiles) for its parent
    files.filterNot(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {
      lsEntryById += file.getId -> new LsEntry(false, file.getId, file)
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
    val rootMyDir = new MyDir("/", rootDirId, childGFilesById(rootDirId))

    myDirById += (rootDirId -> rootMyDir)
    Option(rootChildDirIds).foreach(x => {dirIdQueue ++= x})

    // This is based on assumption that the directory structure follows a tree-like structure
    // Although I am not 100% sure this is the case, AFAIK unix filesystems prevent this
    // https://unix.stackexchange.com/a/22406
    // Basically creating directory structure via BFS
    while (!dirIdQueue.isEmpty) {
      val dirId = dirIdQueue.dequeue()
      val gfile = dirGFileById(dirId)

      // Create new MyDir & add it to map such that (dirId -> MyDir)
      val newMyDir = new MyDir(gfile.getName, gfile.getId, util.Try(childGFilesById(dirId)).getOrElse(ListBuffer()))
      myDirById += dirId -> newMyDir

      // Add children dirs to queue
      util.Try(childDirIdsById(dirId)).foreach(childDirIds => {dirIdQueue ++= childDirIds})
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

    // Populate lsEntries for each MyDir
    myDirById.values.foreach(myDir => {
      val childDirEntries: ListBuffer[LsEntry] = myDir.childDirs.map(childDir => { lsEntryById(childDir.id) })
      val childFileEntries: ListBuffer[LsEntry] = myDir.childFiles.map(childFile => { lsEntryById(childFile.getId) })

      myDir.lsEntries ++= childDirEntries
      myDir.lsEntries ++= childFileEntries
    })

    rootMyDir
  }
}
