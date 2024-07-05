package fr.esgi.al.funprog

import progfun.{Cardinal, Mower, Wore}

import scala.annotation.tailrec
import scala.sys.exit


final case class GardenState(map: List[List[Boolean]], wore: Wore)
@main
def Main(): Unit = {
  println("Ici le programme principal")

  val content = List("5 5", " 1 2 N ", "GAGAGAGAA")
  val map: List[List[Boolean]] = content match {
    case head :: tail => Garden.createGarden(head)
    // Traitement à effectuer
    case Nil => exit(200)
    // Traitement pour une liste vide
  }

  val wore: Wore = content match {
    case head :: second :: tail =>
      Garden.createWoreFromString(
        second
      )
    case Nil =>
      exit(100)
    case List(_) => exit(1001)
  }
  //val path = "AADAADADDA"
   val path = "GAGAGAGAA"
  val gardenState = GardenState(map, wore)
  val result = recursivite(path, gardenState)
  println(result)

}

def readTxt(listString: List[String]): Unit = {
  val mapString = listString match {
    case head :: tail => head
    case  _ => exit(100)
  }
  val garden = Garden.createGarden(mapString)
  val listContent = CreateListWithNoGarden(listString)

}

def CreateListWore(listString: List[String]): List[String] = {
  listString match {
    case head :: second :: tail => List(head, second)
    case head :: second :: Nil => List(head, second)
    case _ => exit(300)
  }
}


/*def createPosition(position: String): Wore = {
  val parts : List[String]  = position.split(" ").toList
  Wore(
    parts[0].toInt,
    parts[1].toInt
    createCardinal(parts(2))
  )
}*/





def CreateListFinish(list: List[String]) : List[String] = {
  list match {
    case head :: second :: tail => tail
    case head :: second :: Nil => List.empty[String]
    case _ => exit(600)
  }
}

def CreateListWithNoGarden(list: List[String]): List[String] = {
  list match {
    case head :: tail => tail
    case _ => exit(200)
  }
}

@tailrec
def recursivite(path: String, gardenState: GardenState): GardenState = {
  println(
    s"test ${gardenState.wore.x}  ${gardenState.wore.y} ${gardenState.wore.orientation}"
  )
  if (path.isEmpty) {
    val map = initialisation(gardenState.wore, gardenState.map)
    GardenState(map, gardenState.wore)
  } else {
    val command = path.head
    val updatedGardenState = action(command, gardenState)
    recursivite(path.tail, updatedGardenState)
  }
}

object Garden {
  def createGarden(input: String): List[List[Boolean]] = {
    val size: Int = Option(input).map(s => s.trim.nn.length).getOrElse(0)
    if (size == 3) then
      val length = createSide(input(0).toString)
      val height = createSide(input(2).toString)
      List.fill(length)(List.fill(height)(false))
    else sys.exit(200)
  }

  private def createSide(input: String): Int = {
    try {
      input.toInt
    } catch {
      case e: NumberFormatException =>
        println(s"Impossible de convertir '$input' en entier.")
        exit(500)
    }
  }

  def createWoreFromString(input: String): Wore = {
    //val parts = List("3", "3", "E") // 3 3 E//1 2 N
    val parts = List("1", "2", "N") //3 3 E//1 2 N
    //val parts = position.split(" ")
    //Mower(Point(parts(0).toInt, parts(1).toInt), parseOrientation(parts(2)))
    if (parts.nn.length != 3)
      sys.exit(200)
    parts match {
      case head :: second :: three :: tail =>
        try {
          val x = head.toInt
          val y = second.toInt

          new Wore(x, y, createManifest(three))
        } catch {
          case e: NumberFormatException =>
            println("Les coordonnées doivent être des entiers.")
            sys.exit(500)
        }
      case List(_, _) => exit(300)
      case List(_)    => exit(600)
      case Nil        => exit(200)
    }

  }


  private def createManifest(input: String): Cardinal = {
    input match {
      case "N"       => Cardinal.N
      case "E"       => Cardinal.E
      case "W"       => Cardinal.W
      case "S"       => Cardinal.S
      case _: String => exit(0)
    }
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
      val wore = changeOrientationWore(Mower.L, gardenState.wore)
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
