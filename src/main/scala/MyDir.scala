import scala.collection.mutable.ListBuffer
import com.google.api.services.drive.model.{File => GFile}

class MyDir (val gfile: GFile, val childFiles: ListBuffer[GFile], val isRootDir: Boolean) {
  val childDirs = ListBuffer[MyDir]()

  def id = gfile.getId
  def name = if (isRootDir) "/" else gfile.getName
  def getName = gfile.getName

  override def hashCode: Int = gfile.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: MyDir => this.isInstanceOf[MyDir] && this.hashCode() == obj.hashCode()
      case _ => false
    }
  }

  override def toString: String = {
    //s"MyDir($name, $id, " + childDirs.toString() + ", " + lsEntries.toString() + ")"
    s"MyDir($name, $id, " + childDirs.toString()  + ")"
  }
}
