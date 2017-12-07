import scala.collection.mutable.ListBuffer
import com.google.api.services.drive.model.{File => GFile}

class MyDir (val name: String, val id: String, val childFiles: ListBuffer[GFile]) {
  val childDirs = ListBuffer[MyDir]()
  val lsEntries = ListBuffer[LsEntry]()

  def getName = name

  override def toString: String = {
    //s"MyDir($name, $id, " + childDirs.toString() + ", " + lsEntries.toString() + ")"
    s"MyDir($name, $id, " + childDirs.toString()  + ")"
  }
}
