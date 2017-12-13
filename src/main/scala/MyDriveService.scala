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

import scala.annotation.tailrec

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

  /** Takes in a Drive instance and returns complete list of files
   *
   *  @param service Drive instance
   *  @return a list of GFiles
   */
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

      // TODO: webContentLink, webViewLink
      val result = service.files.list
        .setPageSize(1000)
        .setPageToken(pageToken)
        .setFields("nextPageToken, files(kind, id, name, parents, mimeType, size, trashed, fullFileExtension, fileExtension, starred, modifiedTime, permissions)")
        .execute()
      totalList ++= result.getFiles.asScala
      pageToken = result.getNextPageToken
    } while (pageToken != null)

    if (PRINT) {
      println()
    }

    // Exclude trashed files
    totalList.filterNot(_.getTrashed)
  }

  /**
    * Takes in the complete list of files and returns the root MyDir
    * @param totalFileList complete list of files
    * @param rootDirGFile GFile instance of root directory
    * @return root MyDir
    */
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
      // Option[java.util.List[String]] -> java.util.List[String] -> List[String]
      Option(file.getParents).foreach(_.asScala.foreach(parent => {
          if (!childDirIdsById.contains(parent)) childDirIdsById += (parent -> new ListBuffer[String]())

          childDirIdsById(parent) += file.getId
      }))
    })

    // Add each file (file ID) to the list of files (GFiles) for its parent
    files.filterNot(_.getMimeType == "application/vnd.google-apps.folder").foreach(file => {
      lsEntryById += file.getId -> new LsEntry(false, file.getId, file)
      // Option[java.util.List[String]] -> java.util.List[String] -> List[String]
      Option(file.getParents).foreach(_.asScala.foreach(parent => {
        if (!childGFilesById.contains(parent)) childGFilesById += (parent -> new ListBuffer[GFile]())

        childGFilesById(parent) += file
      }))
    })

    val dirIdQueue = Queue[String]()
    val myDirById = scala.collection.mutable.Map[String, MyDir]()

    val rootChildDirIds = childDirIdsById(rootDirId)
    val rootMyDir = new MyDir("/", rootDirId, childGFilesById(rootDirId))

    // Add root directory to queue as starting point
    myDirById += (rootDirId -> rootMyDir)
    Option(rootChildDirIds).foreach(x => {dirIdQueue ++= x})

    /* This is based on assumption that the directory structure follows a tree-like structure
     * Although I am not 100% sure this is the case, AFAIK unix filesystems prevent this
     * https://unix.stackexchange.com/a/22406
     *
     * Stated here that while possible in theory it doesn't seem possible to create one
     * So it seems like it would be impossible with the web interface or Android
     * So making the assumption that cycles will not occur
     * https://unix.stackexchange.com/a/22406
     *
     * Basically using BFS to create the directory tree
     */
    while (!dirIdQueue.isEmpty) {
      val dirId = dirIdQueue.dequeue()
      val gfile = dirGFileById(dirId)

      // Create new MyDir & add it to map such that (dirId -> MyDir)
      val newMyDir = new MyDir(gfile.getName, dirId, util.Try(childGFilesById(dirId)).getOrElse(ListBuffer()))
      myDirById += dirId -> newMyDir

      // Add children dirs to dirIdQueue
      util.Try(childDirIdsById(dirId)).foreach(childDirIds => {dirIdQueue ++= childDirIds})
    }

    // Add each MyDir to its parents (excluding the root directory)
    val dirIdsNotRoot = myDirById.keys.filterNot(_.equals(rootDirId))
    // Option[java.util.List[String]] -> java.util.List[String] -> List[String]
    dirIdsNotRoot.foreach(dirId => Option(dirGFileById(dirId).getParents).foreach(_.asScala.foreach(parent => myDirById(parent).childDirs += myDirById(dirId))))

    // Populate lsEntries for each MyDir
    myDirById.values.foreach(myDir => {
      val childDirEntries: ListBuffer[LsEntry] = myDir.childDirs.map(childDir => { lsEntryById(childDir.id) })
      val childFileEntries: ListBuffer[LsEntry] = myDir.childFiles.map(childFile => { lsEntryById(childFile.getId) })

      myDir.lsEntries ++= childDirEntries
      myDir.lsEntries ++= childFileEntries
    })

    rootMyDir
  }

  sealed trait VerifyPathFailure
  final object DoubleDotPlacementFailure extends VerifyPathFailure
  final object DoubleDotCountFailure extends VerifyPathFailure
  final case class InvalidPathFailure(val dirName: String) extends VerifyPathFailure
  final case class EmptyFollowingPathFailure(val curDir: MyDir) extends VerifyPathFailure
  final case class DuplicateNameFailure(val dirName: String) extends VerifyPathFailure

  /** Verifies that a path is correct up to second-to-last directory
   *  (with last dir/file not being checked)
   *
   * @param path a path String
   * @return an instance of VerifyPathFailure or a tuple with last-mile file/directory
   *      2nd-to-last dir (as MyDir), directory list, and a boolean (for leading slash)
   */
  def verifyPath(rootMyDir: MyDir, curMyDir: MyDir, myDirStack: MyDirStack, path: String): Either[VerifyPathFailure, (String, MyDir, List[MyDir], Boolean)] = {
    @tailrec
    def verifyDirs(curDir: MyDir, theLst: List[String], acc: ListBuffer[MyDir], leadingSlash: Boolean):
    Either[VerifyPathFailure, (String, MyDir, List[MyDir], Boolean)] = {
      theLst match {
        case hd :: tl => {
          if (tl.isEmpty) {
            // Having a head but empty tail is base case
            Right((hd.toString, curDir, acc.toList, leadingSlash))
          } else {
            val matchingDirs = curDir.childDirs.filter(_.name == hd)

            if (matchingDirs.length == 0) {
              // Invalid path, found no directories
              Left(InvalidPathFailure(hd))
            } else if (matchingDirs.length == 1) {
              // Found exactly one directory with same name
              verifyDirs(matchingDirs.head, tl, acc += curDir, leadingSlash)
            } else {
              // TODO: Handle multiple directories with the same name
              // Found mutiple directories
              Left(DuplicateNameFailure(hd))
            }
          }
        }
        // This only happens if the initial call starts with theLst as Nil
        // So path must have started with / or ..
        case Nil => {
          Left(EmptyFollowingPathFailure(curDir))
        }
      }
    }

    /** Removes and counts all ..s
     *
     * @param theLst directory listing
     * @param acc accumulator for .. count
     * @return directory listing with ..s removed, and number of ..s
     */
    @tailrec
    def removeAndCountDoubleDots(theLst: List[String], acc: Int): (List[String], Int) = {
      theLst match {
        case Nil => (Nil, acc)
        case hd :: tl => {
          // Because of checks, string starts with bunch of leading ..s, ends with not ..s
          // So we can stop once we no longer find a ..
          if (hd != "..") {
            (tl, acc)
          } else {
            removeAndCountDoubleDots(tl, acc+1)
          }
        }
      }
    }

    // Check if we have ..s after some path (aka ..s not leading) as we only support leading ..s

    /*
     * Split the path by slashes and then filter out any empty elements
     * Then find the first index where there isn't a ..
     */
    val pathSplit = path.split("/").filterNot(_.isEmpty)
    val idxNotDoubleDot = pathSplit.indexWhere(_ != "..")

    if (idxNotDoubleDot != -1 && pathSplit.indexWhere(_ == "..", idxNotDoubleDot) != -1) {
      // If we find a .. after finding something that's not ..
      // we return a failure
      Left(DoubleDotPlacementFailure)
    } else {
      // Check if path starts with leading ..s
      if (path.length == 2 && path(0) == '.' && path(1) == '.' ||
        path.length > 2 && path(0) == '.' && path(1) == '.' && path(2) == '/') {
        // Remove and count the ..s
        val lstCountTuple = removeAndCountDoubleDots(path.slice(0, path.length).split("/").toList, 0)

        val pathLst = lstCountTuple._1
        val doubleDotCount = lstCountTuple._2

        if (doubleDotCount > myDirStack.length) {
          // Too many ..s and we return a failure
          Left(DoubleDotCountFailure)
        } else {
          // Otherwise pop the stack for each ..
          var curDir: MyDir = null
          for (_ <- 1 to doubleDotCount) {
            myDirStack.pop
          }

          // Peek off stack if myDirStack isn't empty
          // Else get rootMyDir
          curDir = if (!myDirStack.isEmpty) myDirStack.peek else rootMyDir

          // Now verify the path
          verifyDirs(curDir, pathLst, ListBuffer[MyDir](), false)
        }
      } else {
        val tuple = path(0) match {
          // If leading slash we're verifying from root directory
          case '/' => (rootMyDir, true)
          // If no leading slash we're verifying from the current directory
          case _ => (curMyDir, false)
        }

        // Verify the path
        verifyDirs(tuple._1, pathSplit.toList, ListBuffer[MyDir](), tuple._2)
      }
    }
  }

}
