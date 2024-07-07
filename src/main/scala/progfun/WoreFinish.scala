package progfun

import upickle.legacy.{macroW, ReadWriter, Writer}

final case class WoreFinish(
    debut: WoreOrientation,
    instruction: List[Char],
    finish: WoreOrientation
) derives ReadWriter

object WoreFinish {
  implicit val woreFinishWriter: Writer[WoreFinish] = macroW[WoreFinish]
}
