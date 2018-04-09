package Drive.Util

import com.google.api.services.drive.model.{File => GFile}

case class LsEntry(val isDir: Boolean, val name: String, val gfile: GFile) {

}
