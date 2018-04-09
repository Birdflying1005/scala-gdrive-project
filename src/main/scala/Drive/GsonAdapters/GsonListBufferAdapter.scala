package Drive.GsonAdapters

import com.google.gson._
import com.google.gson.reflect.TypeToken
import com.google.api.services.drive.model.{File => GFile}
import java.lang.reflect.{ParameterizedType, Type}
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

case class GsonListBufferAdapter() extends JsonSerializer[ListBuffer[_]] with JsonDeserializer[ListBuffer[_]] {
  def deserialize(jsonElement: JsonElement, typeOfT: Type, context: JsonDeserializationContext): ListBuffer[_] = {
    val parameterizedType = ParameterizedTypeImpl.make(classOf[java.util.List[_]], typeOfT.asInstanceOf[ParameterizedType].getActualTypeArguments, null)
    val javaList: java.util.List[_ <: Any] = context.deserialize(jsonElement, parameterizedType)
    javaList.asScala.to[ListBuffer]
  }

  override def serialize(lst: ListBuffer[_], typeOfT: Type, context: JsonSerializationContext): JsonElement = {
    val parameterizedType = ParameterizedTypeImpl.make(classOf[java.util.List[_]], typeOfT.asInstanceOf[ParameterizedType].getActualTypeArguments, null)
    context.serialize(lst.asInstanceOf[ListBuffer[Any]].asJava, parameterizedType)
  }
}
