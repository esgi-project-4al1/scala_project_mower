package progfun

enum Cardinal {
  case N
  case E
  case W
  case S

  override def toString: String = this match {
    case N => "N"
    case E => "E"
    case W => "W"
    case S => "S"
  }
}
