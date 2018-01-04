import com.google.gson._
import com.google.gson.reflect.TypeToken
import com.google.api.services.drive.model.{File => GFile, Permission}
import com.google.api.services.drive.model.Permission.TeamDrivePermissionDetails
import com.google.api.client.util.DateTime
import java.lang.reflect.Type
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

case class GsonGFileAdapter() extends JsonDeserializer[GFile] {
  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext) = {
    val jsonObj = json.getAsJsonObject
    val gfile = new GFile()
    //gfile.set(Option(jo.get("")).map().getOrElse(null))

    gfile.setKind(Option(jsonObj.get("kind")).map(_.getAsString).getOrElse(null))
    gfile.setId(Option(jsonObj.get("id")).map(_.getAsString).getOrElse(null))
    gfile.setName(Option(jsonObj.get("name")).map(_.getAsString).getOrElse(null))
    gfile.setParents(Option(jsonObj.get("parents")).map(elem => {
      val jsonArray: JsonArray = elem.getAsJsonArray
      val jsonElems: Iterator[JsonElement] = asScalaIterator(jsonArray.iterator())
      val lstBuf: ListBuffer[String] = ListBuffer[String]()

      for (elem <- jsonElems) {
        lstBuf.append(elem.getAsString)
      }

      lstBuf.asJava
    }).getOrElse(null))
    gfile.setMimeType(Option(jsonObj.get("mimeType")).map(_.getAsString).getOrElse(null))
    gfile.setSize(Option(jsonObj.get("size")).map(_.getAsLong).getOrElse(0) : Long)
    gfile.setTrashed(Option(jsonObj.get("trashed")).map(_.getAsBoolean).getOrElse(false) : Boolean)
    gfile.setFullFileExtension(Option(jsonObj.get("fullFileExtension")).map(_.getAsString).getOrElse(null))
    gfile.setFileExtension(Option(jsonObj.get("fileExtension")).map(_.getAsString).getOrElse(null))
    gfile.setStarred(Option(jsonObj.get("starred")).map(_.getAsBoolean).getOrElse(false) : Boolean)
    //gfile.setOwnedByMe(Option(jsonObj.get("ownedByMe")).map(_.getAsBoolean).getOrElse(true) : Boolean)
    gfile.setPermissions(Option(jsonObj.get("permissions")).map(elem => {
      val jsonArray: JsonArray = elem.getAsJsonArray
      val jsonElems: Iterator[JsonElement] = asScalaIterator(jsonArray.iterator())
      val lstBuf: ListBuffer[Permission] = ListBuffer[Permission]()

      for (elem <- jsonElems) {
        val permJsonObj = elem.getAsJsonObject
        val permission = new Permission()

        permission.setKind(Option(permJsonObj.get("kind")).map(_.getAsString).getOrElse(null))
        permission.setId(Option(permJsonObj.get("id")).map(_.getAsString).getOrElse(null))
        permission.setType(Option(permJsonObj.get("type")).map(_.getAsString).getOrElse(null))
        permission.setEmailAddress(Option(permJsonObj.get("emailAddress")).map(_.getAsString).getOrElse(null))
        permission.setDomain(Option(permJsonObj.get("domain")).map(_.getAsString).getOrElse(null))
        permission.setRole(Option(permJsonObj.get("role")).map(_.getAsString).getOrElse(null))
        permission.setDisplayName(Option(permJsonObj.get("displayName")).map(_.getAsString).getOrElse(null))
        permission.setPhotoLink(Option(permJsonObj.get("photoLink")).map(_.getAsString).getOrElse(null))
        permission.setAllowFileDiscovery(Option(permJsonObj.get("allowFileDiscovery")).map(_.getAsBoolean).getOrElse(false) : Boolean)
        permission.setDeleted(Option(permJsonObj.get("deleted")).map(_.getAsBoolean).getOrElse(false) : Boolean)
        permission.setExpirationTime(Option(permJsonObj.get("expirationTime")).map(elem => {
          val jsonObj = elem.getAsJsonObject
          val dateOnly: Boolean = Option(jsonObj.get("dateOnly")).map(_.getAsBoolean).getOrElse(true)
          val value: Long = Option(jsonObj.get("value")).map(_.getAsLong).getOrElse(0L)
          val tzShift: Integer = (Option(jsonObj.get("tzShift")).map(_.getAsInt).getOrElse(0)).toInt

          new DateTime(dateOnly, value, tzShift)
        }).getOrElse(null))
        permission.setTeamDrivePermissionDetails(Option(permJsonObj.get("teamDrivePermissionDetails")).map(obj => {
          val jsonArray: JsonArray = obj.getAsJsonArray
          val jsonElems: Iterator[JsonElement] = asScalaIterator(jsonArray.iterator())
          val lstBuf: ListBuffer[TeamDrivePermissionDetails] = ListBuffer[TeamDrivePermissionDetails]()

          for (elem <- jsonElems) {
            val tdpdJsonObj = elem.getAsJsonObject
            val tdpd = new TeamDrivePermissionDetails()

            tdpd.setTeamDrivePermissionType(Option(tdpdJsonObj.get("teamDrivePermissionType")).map(_.getAsString).getOrElse(null))
            tdpd.setRole(Option(tdpdJsonObj.get("role")).map(_.getAsString).getOrElse(null))
            tdpd.setInheritedFrom(Option(tdpdJsonObj.get("inheritedFrom")).map(_.getAsString).getOrElse(null))
            tdpd.setInherited(Option(tdpdJsonObj.get("inherited")).map(_.getAsBoolean).getOrElse(false) : Boolean)

            lstBuf.append(tdpd)
          }

          lstBuf.asJava
        }).getOrElse(null))

        lstBuf.append(permission)
      }

      lstBuf.asJava
    }).getOrElse(null))
    gfile.setModifiedTime(Option(jsonObj.get("modifiedTime")).map(elem => {
      val jsonObj = elem.getAsJsonObject
      val dateOnly: Boolean = Option(jsonObj.get("dateOnly")).map(_.getAsBoolean).getOrElse(true)
      val value: Long = Option(jsonObj.get("value")).map(_.getAsLong).getOrElse(0L)
      val tzShift: Integer = (Option(jsonObj.get("tzShift")).map(_.getAsInt).getOrElse(0)).toInt

      new DateTime(dateOnly, value, tzShift)
    }).getOrElse(null))

    gfile
  }

  /*
  def serialize(obj: GFile, t: Type, jdc: JsonSerializationContext): JsonElement = {
    val gson = new Gson()
    val gType = new TypeToken[GFile]() {}.getType()
    gson.toJson(obj, gType)
  }
  */
}
