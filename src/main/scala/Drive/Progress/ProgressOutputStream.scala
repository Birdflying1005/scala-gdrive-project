package Drive.Progress

import java.io.{FilterOutputStream, OutputStream}

class ProgressOutputStream(out: OutputStream, listener: Long => Unit) extends FilterOutputStream(out) {
  val NotificationThreshold = 8 * 1024;

  var unnotifiedByteCount = 0

  override def write(byte: Int): Unit = {
    super.write(byte)
    notify(1)
  }

  override def write(bytes: Array[Byte], off: Int, len: Int): Unit = {
    super.write(bytes, off, len)
    notify(len)
  }

  override def write(bytes: Array[Byte]): Unit = {
    super.write(bytes)
    notify(bytes.length)
  }

  override def close() {
    if (unnotifiedByteCount > 0) {
      listener(unnotifiedByteCount)
      unnotifiedByteCount = 0
    }

    super.close()
  }

  def notify(bytesWritten: Int) {
    unnotifiedByteCount += bytesWritten

    if (unnotifiedByteCount >= NotificationThreshold) {
      listener(unnotifiedByteCount)
      unnotifiedByteCount = 0
    }
  }
}

object ProgressOutputStream {
  def apply(out: OutputStream, availableBytes: Long, tracker: Progress => Unit) =
    new ProgressOutputStream(out, new ProgressListener(availableBytes, tracker))
}
