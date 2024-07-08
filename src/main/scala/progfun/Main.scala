package progfun

import ConfigReader._
import better.files.File

import upickle.legacy.write

import scala.annotation.tailrec
import scala.io.StdIn
import scala.sys.exit

final case class GardenState(map: List[List[Boolean]], wore: Wore)

@main
def Main(): Unit = {
  println(
    ":> Enter the path to the config JSON file (or press Enter to use default config.json): "
  )
  val configFilePath = StdIn.readLine()
  val defaultConfigPath = "config.json"
  val configPath =
    if (configFilePath.isEmpty) defaultConfigPath else configFilePath

  val config = ConfigReader.readConfig(configPath)
  runWithConfig(config)
}

def runWithConfig(config: Config): Unit = {
  println(":> Enter streaming for streaming mode: ")
  val args = StdIn.readLine()
  args match {
    case "streaming" => {
      runModeStreaming(config)
    }
    case _ => {
      startModeFull(config)
    }
  }
}

def startModeFull(config: Config): Unit = {
  val f = File(config.inputPath)
  start(f.lines.toList, config)
}

def createFileJson(contentFile: ContentFile, path: String): Unit = {
  val file = File(path).createIfNotExists()
  val jsonString = write[ContentFile](contentFile)
  file.overwrite(jsonString)
}

def start(line: List[String], config: Config): Unit = {
  val result = startWore(line)
  display(result)
  val resultYAML = convertToYaml(result)
  writeToYAMLFile(resultYAML, config.yamlPath)
  val resultCSV = convertToCsv(result)
  writeToYAMLFile(resultCSV, config.csvPath)
  createFileJson(result, config.jsonPath)
}

def startWore(line: List[String]): ContentFile = {
  val point: Point = line match {
    case head :: tail => parseGarden(head)
    case Nil          => exit(200)
  }
  val map = List.fill(point.x + 1)(List.fill(point.y + 1)(false))
  val restWithNoGarden = CreateListWithNoGarden(line)
  val sousListes = restWithNoGarden.grouped(2).toList
  val listWoreWithContent =
    sousListes.map(content => createWoreWithContent(content))
  val listWoreWithContentReverse =
    recursive(listWoreWithContent, map, List.empty[WoreFinish]).reverse
  ContentFile(limite = point, tondeuses = listWoreWithContentReverse)
}

def display(contentFile: ContentFile): Unit = {
  for (wore <- contentFile.tondeuses) {
    println(
      s"${wore.debut.point.x}  ${wore.debut.point.y} ${wore.debut.direction}"
    )
    println(
      s"${wore.fin.point.x}  ${wore.fin.point.y} ${wore.fin.direction}"
    )
    println(s"${wore.instructions}")
  }
}

@tailrec
def recursive(
    listWoreWithContent: List[WoreWithContent],
    map: List[List[Boolean]],
    listWoreFinish: List[WoreFinish]): List[WoreFinish] = {
  listWoreWithContent match {
    case head :: tail => {
      val gardenState = run(head, map)
      val woreFinish =
        createWoreFinish(head.wore, gardenState.wore, head.content)
      recursive(tail, gardenState.map, woreFinish :: listWoreFinish)
    }
    case Nil => listWoreFinish
  }
}

def createWoreFinish(
    woreStart: Wore,
    woreFinish: Wore,
    instructionString: String): WoreFinish = {
  val instruction = createListCharWithString(instructionString)
  val debut =
    WoreOrientation(
      Point(woreStart.x, woreStart.y),
      woreStart.orientation.toString
    )
  val finish =
    WoreOrientation(
      Point(woreFinish.x, woreFinish.y),
      woreFinish.orientation.toString
    )
  WoreFinish(debut, instruction, finish)
}

def run(
    woreWithContent: WoreWithContent,
    map: List[List[Boolean]]): GardenState = {
  val gardenState = GardenState(map, woreWithContent.wore)
  recursivite(woreWithContent.content, gardenState)
}

def createWoreWithContent(list: List[String]): WoreWithContent = {
  val woreListContent = CreateListWore(list)
  val wore = woreListContent match {
    case head :: tail => createWore(head)
    case _            => exit(300)
  }
  val content = woreListContent match {
    case head :: second :: tail => second
    case _                      => exit(300)
  }
  WoreWithContent(wore, content)
}

def createWore(input: String): Wore = {
  val woreListChar = createListCharWithString(input)
  val listInt = groupOptions(createListIsInt(woreListChar))
  val cardinal = mapListIntoCardinal(
    groupOptionsString(createListIsString(woreListChar))
  )
  listInt match {
    case head :: second :: Nil => Wore(head.toInt, second.toInt, cardinal)
    case _                     => exit(200)
  }

}

def CreateListWore(listString: List[String]): List[String] = {
  listString match {
    case head :: second :: Nil  => List(head, second)
    case head :: second :: tail => List(head, second)
    case _                      => exit(300)
  }
}

def createListCharWithString(input: String): List[Char] = {
  input.toList
}

def createListIsInt(charList: List[Char]): List[Option[Int]] = {

  for {
    c <- charList
  } yield {
    if (c.isDigit) {
      Option(c.toString.toInt)
    } else {
      Option.empty[Int]
    }
  }

}

def createListIsString(input: List[Char]): List[Option[String]] = {
  for {
    c <- input
  } yield {
    if (c == 78 || c == 69 || c == 87 || c == 83) {
      Option(c.toString)
    } else {
      Option.empty[String]
    }
  }

}

def groupOptions(list: List[Option[Int]]): List[String] = {
  list
    .foldRight(List[List[Option[Int]]]()) { (current, acc) =>
      current match {
        case Some(_) =>
          acc match {
            case head :: tail => (current :: head) :: tail
            case Nil          => List(List(current))
          }
        case None => List.empty :: acc
      }
    }
    .filter(_.nonEmpty)
    .map(_.flatten)
    .map(_.map(_.toString).mkString)
}

def CreateListFinish(list: List[String]): List[String] = {
  list match {
    case head :: second :: tail => tail
    case _                      => exit(600)
  }
}

def mapListIntoCardinal(list: List[String]): Cardinal = {
  list match {
    case head :: Nil => createCardinal(head.charAt(0))
    case _           => exit(230)
  }

}

def CreateListWithNoGarden(list: List[String]): List[String] = {
  list match {
    case head :: tail => tail
    case _            => exit(200)
  }
}

def groupOptionsString(list: List[Option[String]]): List[String] = {
  list
    .foldRight(List[List[Option[String]]]()) { (current, acc) =>
      current match {
        case Some(_) =>
          acc match {
            case head :: tail => (current :: head) :: tail
            case Nil          => List(List(current))
          }
        case None => List.empty :: acc
      }
    }
    .filter(_.nonEmpty)
    .map(_.flatten)
    .map(_.mkString)
}

@tailrec
def recursivite(path: String, gardenState: GardenState): GardenState = {
  println(
    s"test ${gardenState.wore.x}  ${gardenState.wore.y} ${gardenState.wore.orientation}"
  )
  if (path.isEmpty) {
    println("---next---")
    val map = initialisation(gardenState.wore, gardenState.map)
    GardenState(map, gardenState.wore)
  } else {
    val command = path.head
    val updatedGardenState = action(command, gardenState)
    recursivite(path.tail, updatedGardenState)
  }
}

def parseGarden(input: String): Point = {
  val gardenListChar = createListCharWithString(input)
  val listInt = groupOptions(createListIsInt(gardenListChar))
  listInt match {
    case head :: second :: Nil => Point(head.toInt, second.toInt)
    case _                     => exit(600)
  }
}

def createSidePlus1(input: String): Int = {
  try {
    input.toInt + 1
  } catch {
    case e: NumberFormatException =>
      println(s"Impossible de convertir '$input' en entier.")
      exit(500)
  }
}

def initialisation(
    wore: Wore,
    garden: List[List[Boolean]]): List[List[Boolean]] = {
  garden.zipWithIndex.map { case (value, index) =>
    if index != wore.x then value
    else initializeList(value.length, wore.y, true)
  }
}

private def initializeList(
    size: Int,
    truePosition: Int,
    add: Boolean): List[Boolean] = {
  if (truePosition >= size || truePosition < 0) {
    exit(200)
  } else {
    List.fill(size)(false).zipWithIndex.map { case (value, index) =>
      if (index == truePosition) add else add != add
    }
  }
}

private def findOneRowInGarderRow(
    y: Int,
    garden: List[Boolean]): Option[Boolean] = {
  garden.lift(y)
}

def findOneRowInGarder(gardenState: GardenState): Option[Boolean] = {
  gardenState.map.lift(gardenState.wore.x) match {
    case Some(row) => findOneRowInGarderRow(gardenState.wore.y, row)
    case None      => Option.empty[Boolean]
  }
}

def supress(wore: Wore, garden: List[List[Boolean]]): List[List[Boolean]] = {
  garden.zipWithIndex.map { case (value, index) =>
    if index != wore.x then value
    else initializeList(value.length, wore.y, false)
  }
}

def action(command: Char, gardenState: GardenState): GardenState = {
  command match {
    case 'G' =>
      val wore = changeOrientationWore(Mower.L, gardenState.wore)
      GardenState(wore = wore, map = gardenState.map)
    case 'D' =>
      val wore = changeOrientationWore(Mower.R, gardenState.wore)
      GardenState(wore = wore, map = gardenState.map)
    case 'A' => {
      moveForward(gardenState)
    }
    case _ => exit(2)
  }
}

def changeOrientationWore(move: Mower, wore: Wore): Wore = {
  val newOrientation = (move, wore.orientation) match {
    case (Mower.L, Cardinal.E) => Cardinal.N
    case (Mower.L, Cardinal.N) => Cardinal.W
    case (Mower.L, Cardinal.S) => Cardinal.E
    case (Mower.L, Cardinal.W) => Cardinal.S
    case (Mower.R, Cardinal.E) => Cardinal.S
    case (Mower.R, Cardinal.N) => Cardinal.E
    case (Mower.R, Cardinal.S) => Cardinal.W
    case (Mower.R, Cardinal.W) => Cardinal.N
    case _                     => sys.exit(2)
  }
  new Wore(wore.x, wore.y, newOrientation)
}

def moveForward(gardenState: GardenState): GardenState = {
  val x =
    gardenState.map.headOption.map(_.length).getOrElse(0) // Width of the garden
  val y = gardenState.map.length // Height of the garden

  val (newX, newY) = gardenState.wore.orientation match {
    case Cardinal.N =>
      (
        gardenState.wore.x,
        Math.min(gardenState.wore.y + 1, y - 1)
      ) // Move north, increase Y but do not exceed garden height
    case Cardinal.S =>
      (
        gardenState.wore.x,
        Math.max(gardenState.wore.y - 1, 0)
      ) // Move south, decrease Y but do not go below 0
    case Cardinal.E =>
      (
        Math.min(gardenState.wore.x + 1, x - 1),
        gardenState.wore.y
      ) // Move east, increase X but do not exceed garden width
    case Cardinal.W =>
      (
        Math.max(gardenState.wore.x - 1, 0),
        gardenState.wore.y
      ) // Move west, decrease X but do not go below 0
  }

  val woreInThisPlace = findOneRowInGarder(gardenState).getOrElse(false)

  if (
    newX != gardenState.wore.x || newY != gardenState.wore.y || woreInThisPlace
  ) {
    GardenState(
      gardenState.map,
      new Wore(newX, newY, gardenState.wore.orientation)
    )
  } else {
    gardenState
  }
}

def createCardinal(orientation: Char): Cardinal = {
  orientation match {
    case 'N' => Cardinal.N
    case 'E' => Cardinal.E
    case 'W' => Cardinal.W
    case 'S' => Cardinal.S
    case _   => exit(100)
  }
}

def convertToYaml(contentFile: ContentFile): String = {
  val limiteYaml =
    s"""limite:
       |  x: ${contentFile.limite.x}
       |  y: ${contentFile.limite.y}""".stripMargin

  val tondeuse = "  tondeuses:"
  val tondeusesYaml = contentFile.tondeuses
    .map { wore =>
      s"""  - debut:
       |      point:
       |        x: ${wore.debut.point.x}
       |        y: ${wore.debut.point.y}
       |      direction: ${wore.debut.direction}
       |    fin: \n${transformListToStringForYaml(wore.instructions)}
       |    fin:
       |      point:
       |        x: ${wore.fin.point.x}
       |        y: ${wore.fin.point.y}
       |      direction: ${wore.fin.direction}""".stripMargin
    }
    .mkString("\n")

  s"$limiteYaml\n$tondeuse\n$tondeusesYaml"
}

def transformListToStringForYaml(instructions: List[Char]): String = {
  instructions
    .map { instruction =>
      s"       - ${instruction}"
    }
    .mkString("\n")
}

def convertToCsv(contentFile: ContentFile): String = {
  val header =
    "numéro;début_x;début_y;début_direction;fin_x;fin_y;fin_direction;fin"
  val rows = contentFile.tondeuses.zipWithIndex.map { case (wore, number) =>
    val instructions = wore.instructions.mkString("")
    s"${number + 1};${wore.debut.point.x};${wore.debut.point.y};${wore.debut.direction};" +
      s"${wore.fin.point.x};${wore.fin.point.y};${wore.fin.direction};$instructions"
  }
  (header :: rows).mkString("\n")
}

def writeToYAMLFile(content: String, path: String): Unit = {
  try {
    val file = File(path)
    file.write(content)
  } catch {
    case e: Exception =>
      e.printStackTrace()
  }

}
