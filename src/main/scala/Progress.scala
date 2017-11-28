import math._

case class Progress(percent: Int, size: Long, remains: Long, done: Long, bps: Long, elapsed: Long, estimated: Long) {
  lazy val formattedMetrics= List(
    "Progress: " + percent + "%",
    "File size: " + formatSize(size),
    "Downloaded: " + formatSize(done),
    "Remains: " + formatSize(remains),
    "Speed: " + (bps / 1024) + " kb/s",
    "Elapsed: " + formatTime(elapsed),
    "Estimated: " + formatTime(estimated)
  )

  def formatTime(time: Long) = {
    val oneSecond = 1000
    val oneMinute = 60 * oneSecond
    val oneHour = 60 * oneMinute

    val hours = (time / oneHour).toInt
    val minutes = ((time - hours * oneHour) / oneMinute).toInt
    val seconds = ((time - hours * oneHour - minutes * oneMinute) / oneSecond).toInt

    val fmt = "%02d"

    List(hours, minutes, seconds) map (fmt format _) mkString ":"
  }

  def formatSize(bytes: Long) =
    if (bytes < 1024)
      bytes + " B"
    else {
      val exp = (log(bytes) / log(1024)).toInt
      "%.1f %sB" format (bytes / pow(1024, exp), "KMGTPE" charAt (exp - 1))
    }
}