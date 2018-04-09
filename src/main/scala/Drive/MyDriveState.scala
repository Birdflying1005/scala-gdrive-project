package Drive

import MyDriveService._
import Drive.Util._
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import com.google.api.services.drive.{Drive => GDrive}
import com.google.api.services.drive.model.{File => GFile}
import com.diogonunes.jcdp.color.ColoredPrinter

object MyDriveState {
  val service: GDrive = getDriveService
  var rootDirGFile: GFile = service.files().get("root").execute()
  var totalFileList: ListBuffer[GFile] = populateTotalFileList(service)
  var directoryStructureTuple: (MyDir, scala.collection.mutable.Map[String, MyDir]) = getDirectoryStructure(totalFileList, rootDirGFile)
  var myDirById: scala.collection.mutable.Map[String, MyDir] = directoryStructureTuple._2
  var rootMyDir: MyDir = directoryStructureTuple._1
  var curMyDir: MyDir = rootMyDir
  val colorPrinter = new ColoredPrinter.Builder(1, false).build()
  val myDirStack = new MyDirStack()
}
