package progfun

import upickle.legacy.{macroW, ReadWriter, Writer}

final case class WoreFinish(
    debut: WoreOrientation,
    instructions: List[Char],
    fin: WoreOrientation
) derives ReadWriter

object WoreFinish {
  implicit val woreFinishWriter: Writer[WoreFinish] = macroW[WoreFinish]
}
