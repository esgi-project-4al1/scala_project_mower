package progfun

import upickle.legacy.{macroW, ReadWriter, Writer}

final case class Point(
    x: Int,
    y: Int
) derives ReadWriter

object Point {
  implicit val pointWriter: Writer[Point] = macroW[Point]
}
