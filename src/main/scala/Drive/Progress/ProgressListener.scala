package Drive.Progress

import math._

class ProgressListener(availableBytes: Long, tracker: Progress => Unit) extends Function1[Long, Unit] {
  var transfered: Long = 0
  val startTime = System.currentTimeMillis

  def apply(bytesTransfered: Long) {
    transfered += bytesTransfered
    val elapsed = System.currentTimeMillis - startTime
    val bpms = (transfered / max(elapsed, 1000)).toLong
    val bps = (transfered / max(elapsed / 1000, 1)).toLong

    tracker(Progress(
      percent = (transfered * 100 / availableBytes).toInt min 100,
      size = availableBytes,
      remains = availableBytes - transfered,
      done = transfered,
      bps = bps,
      elapsed = elapsed,
      estimated = (availableBytes - transfered) / max(bpms, 1)
    ))
  }
}
