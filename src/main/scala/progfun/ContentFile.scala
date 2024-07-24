package progfun

import upickle.legacy.{macroRW, ReadWriter}

import scala.collection.immutable.List

final case class ContentFile(limite: Point, tondeuses: List[WoreFinish])

object ContentFile {
  implicit val rw: ReadWriter[ContentFile] = macroRW
}
