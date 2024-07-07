package progfun

import upickle.legacy.{ReadWriter, Writer, macroW}

final case class WoreOrientation (
  point: Point,
  orientation: String,
)
  derives ReadWriter

object WoreOrientation {
  implicit val woreOrientationWriter: Writer[WoreOrientation] = macroW[WoreOrientation]
}
