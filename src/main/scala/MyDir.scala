import scala.collection.mutable.ListBuffer
import com.google.api.services.drive.model.{File => GFile}

class MyDir (val name: String, val id: String, val childFiles: List[GFile]) {
  val childDirs = ListBuffer[MyDir]()

  override def toString: String = {
    s"MyDir($name, $id, " + childDirs.toString() + ")"
  }
}
