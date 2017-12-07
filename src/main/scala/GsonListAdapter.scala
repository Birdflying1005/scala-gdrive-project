import com.google.gson._
import java.lang.reflect.{ParameterizedType, Type}

// Taken from here: https://stackoverflow.com/a/30746061/2251666
case class GsonListAdapter() extends JsonSerializer[List[_]] with JsonDeserializer[List[_]] {
  import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl
  import scala.collection.JavaConverters._

  @throws(classOf[JsonParseException])
  def deserialize(jsonElement: JsonElement, t: Type, jdc: JsonDeserializationContext): List[_] = {
    val p = scalaListTypeToJava(t.asInstanceOf[ParameterizedType]) // Safe casting because List is a ParameterizedType.
    val javaList: java.util.List[_ <: Any] = jdc.deserialize(jsonElement, p)
    javaList.asScala.toList
  }

  override def serialize(obj: List[_], t: Type, jdc: JsonSerializationContext): JsonElement = {
    val p = scalaListTypeToJava(t.asInstanceOf[ParameterizedType]) // Safe casting because List is a ParameterizedType.
    jdc.serialize(obj.asInstanceOf[List[Any]].asJava, p)
  }

  private def scalaListTypeToJava(t: ParameterizedType): ParameterizedType = {
    ParameterizedTypeImpl.make(classOf[java.util.List[_]], t.getActualTypeArguments, null)
  }
}
