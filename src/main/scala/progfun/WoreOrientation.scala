package progfun

import upickle.legacy.{macroW, ReadWriter, Writer}

final case class WoreOrientation(
    point: Point,
    orientation: String
) derives ReadWriter

object WoreOrientation {
  implicit val woreOrientationWriter: Writer[WoreOrientation] =
    macroW[WoreOrientation]
}
