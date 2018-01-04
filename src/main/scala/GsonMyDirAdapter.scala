import com.google.gson._
import com.google.gson.reflect.TypeToken
import com.google.api.services.drive.model.{File => GFile, Permission}
import com.google.api.services.drive.model.Permission.TeamDrivePermissionDetails
import com.google.api.client.util.DateTime
import java.lang.reflect.Type
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

case class GsonMyDirAdapter() extends JsonDeserializer[MyDir] {
  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): MyDir = {
    val jsonObj = json.getAsJsonObject
    val gfileType = new TypeToken[GFile]() {}.getType()

    val gfile = Option(jsonObj.get("gfile")).map(elem => context.deserialize(elem, gfileType)).getOrElse(null)

    val isRootDir = Option(jsonObj.get("isRootDir")).map(_.getAsBoolean).getOrElse(false) : Boolean

    val childDirs = Option(jsonObj.get("childDirs")).map(elem => {
      val jsonArray: JsonArray = elem.getAsJsonArray
      val jsonElems: Iterator[JsonElement] = asScalaIterator(jsonArray.iterator())
      val lstBuf: ListBuffer[MyDir] = ListBuffer[MyDir]()

      for (elem <- jsonElems) {
        lstBuf.append(context.deserialize(elem, typeOfT))
      }

      lstBuf
    }).getOrElse(null)

    val childFiles = Option(jsonObj.get("childFiles")).map(elem => {
      val jsonArray: JsonArray = elem.getAsJsonArray
      val jsonElems: Iterator[JsonElement] = asScalaIterator(jsonArray.iterator())
      val lstBuf: ListBuffer[GFile] = ListBuffer[GFile]()

      for (elem <- jsonElems) {
        lstBuf.append(context.deserialize(elem, gfileType))
      }

      lstBuf
    }).getOrElse(null)

    val myDirChildFiles = new ListBuffer[GFile]()
    myDirChildFiles ++= childFiles

    val myDir = new MyDir(gfile, myDirChildFiles, isRootDir)
    myDir.childDirs ++= childDirs

    myDir
  }

  /*
  def serialize(obj: GFile, t: Type, jdc: JsonSerializationContext): JsonElement = {
    val gson = new Gson()
    val gType = new TypeToken[GFile]() {}.getType()
    gson.toJson(obj, gType)
  }
  */
}
