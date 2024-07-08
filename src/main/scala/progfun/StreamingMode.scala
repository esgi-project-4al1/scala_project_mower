package progfun

import scala.annotation.tailrec
import scala.io.StdIn.readLine

def runModeStreaming(config: Config): Unit = {
  println("Enter command (press 'enter' to quit): ")
  val result = loop(true, 0, List.empty[String])
  start(result, config)
}

@tailrec
def loop(continue: Boolean, start: Int, list: List[String]): List[String] = {
  if (continue) {
    val input = readLine()
    input match {
      case "" =>
        list.reverse
      case content: String =>
        loop(true, start + 1, content :: list)
    }
  } else {
    list
  }
}

def createGarderPoint(content: String): Point = {
  parseGarden(content)
}

def createGarden(point: Point): List[List[Boolean]] = {
  List.fill(point.x + 1)(List.fill(point.y + 1)(false))
}

def runWore(
    wore: Wore,
    instruction: String,
    garden: List[List[Boolean]]): WoreFinish = {
  val woreWithContent = WoreWithContent(wore, instruction)
  val gardenState = GardenState(garden, woreWithContent.wore)
  val result = recursivite(woreWithContent.content, gardenState)
  createWoreFinish(wore, result.wore, instruction)
}
