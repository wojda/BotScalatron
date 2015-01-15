// Example Bot #1: The Reference Bot


class MiniBot extends Bot{

  def react(command: CommandFromServer): String = {
    val botAction = new BotActionActionImpl(command)
    val (directionValue, nearestEnemyMaster, nearestEnemySlave) = analyzeViewAsMiniBot(botAction.view)

    if(botAction.energy > 300) botAction.spawn(XY.Up)

    if(nearestEnemyMaster.nonEmpty && nearestEnemyMaster.get < 8) {
      if (nearestEnemyMaster.get <= 2) botAction.explode(4)
      else botAction.move(botAction.view.offsetToNearest(Cell.WITH_ENEMY_BOT).get.signum)

    } else if(nearestEnemySlave.nonEmpty && nearestEnemySlave.get <= 4) {
      if (nearestEnemySlave.get <= 3) botAction.explode(4)
      else botAction.move(botAction.view.offsetToNearest(Cell.WITH_ENEMY_MINI_BOT).get.signum)

    } else {

      // determine movement direction
      directionValue(botAction.lastDirection) += 10 // try to break ties by favoring the last direction
      val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
      val direction = XY.fromDirection45(bestDirection45)
      botAction.move(direction)
      botAction.set("lastDirection" -> bestDirection45)
    }

    botAction.toString
  }


  def analyzeViewAsMiniBot(view: View) = {
    val directionValue = Array.ofDim[Double](8)
    var enemyMasterStepDistance: Option[Int] = None
    var enemySlaveStepDistance: Option[Int] = None

    val cells = view.cells
    val cellCount = cells.length

    for (i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case Cell.WITH_ENEMY_BOT.symbol =>
            enemyMasterStepDistance = Some(stepDistance)
            1000
          case Cell.WITH_ENEMY_MINI_BOT.symbol =>
            enemySlaveStepDistance = Some(stepDistance)
            100 / stepDistance

          case Cell.WITH_MY_MINI_BOT.symbol =>
            -50

          case Cell.WITH_EDIBLE_BEAST.symbol =>
            if (stepDistance == 1) 600
            else if (stepDistance == 2) 300
            else (150 - stepDistance * 15).max(10)

          case Cell.WITH_EDIBLE_PLANT.symbol =>
            if (stepDistance == 1) 500
            else if (stepDistance == 2) 300
            else (150 - stepDistance * 10).max(10)

          case Cell.WITH_PREDATOR_BEAST.symbol =>
            if (stepDistance < 4) -400 / stepDistance else -50 / stepDistance

          case Cell.WITH_POISONOUS_PLANT.symbol =>
            if (stepDistance < 2) -1000 else 0

          case Cell.WITH_WALL.symbol =>
            if (stepDistance < 2) -1000 else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    (directionValue, enemyMasterStepDistance, enemySlaveStepDistance)
  }
}


class MasterBot extends Bot{

  def react(command: CommandFromServer): String = {
    val botAction = new BotActionActionImpl(command)

    val directionValue = analyzeViewAsMaster(botAction.view)

    // determine movement direction
    directionValue(botAction.lastDirection) += 10 // try to break ties by favoring the last direction

    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val direction = XY.fromDirection45(bestDirection45)
    botAction.move(direction)
    botAction.set("lastDirection" -> bestDirection45)

    if (botAction.energy > 300) {
      botAction.spawn(direction.rotateClockwise45, "mood" -> "Aggressive")
    }

    botAction.toString
  }


  /** Analyze the view, building a map of attractiveness for the 45-degree directions and
    * recording other relevant data, such as the nearest elements of various kinds.
    */
  private def analyzeViewAsMaster(view: View) = {
    val directionValue = Array.ofDim[Double](8)

    val cells = view.cells
    val cellCount = cells.length
    for(i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if(cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {

          case Cell.WITH_ENEMY_BOT.symbol =>
            if(stepDistance < 6) -1000 else 0

          case Cell.WITH_ENEMY_MINI_BOT.symbol =>
            -100 / stepDistance

          case Cell.WITH_MY_MINI_BOT.symbol =>
            0.0

          case Cell.WITH_EDIBLE_BEAST.symbol =>
            if(stepDistance == 1) 600
            else if(stepDistance == 2) 300
            else (150 - stepDistance * 15).max(10)

          case Cell.WITH_EDIBLE_PLANT.symbol =>
            if(stepDistance == 1) 500
            else if(stepDistance == 2) 300
            else (150 - stepDistance * 10).max(10)

          case Cell.WITH_PREDATOR_BEAST.symbol =>
            if(stepDistance < 4) -400 / stepDistance
            else -50 / stepDistance

          case Cell.WITH_POISONOUS_PLANT.symbol =>
            if(stepDistance < 2) -1000 else 0

          case Cell.WITH_WALL.symbol =>
            if(stepDistance < 2) -1000 else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }

    directionValue
  }
}



// -------------------------------------------------------------------------------------------------
// Framework
// -------------------------------------------------------------------------------------------------

class ControlFunctionFactory {

  def create = (input: String) => {
    CommandFromServer(input) match {

      case Some(command) =>
        val bot = BotFactory(command)
        bot.react(command)
      case None =>
        "" //do nothing
        
    }
  }
}



object BotFactory {
  def apply(command: CommandFromServer): Bot = {
    if(command.isForMasterBot) new MasterBot
    else new MiniBot
  }
}


trait Bot {
  def react(command: CommandFromServer): String
}



trait BotAction {
  // inputs
  def view: View
  def energy: Int
  def time: Int
  def generation: Int
  def lastDirection: Int

  // outputs
  def move(delta: XY) : BotAction
  def say(text: String) : BotAction
  def status(text: String) : BotAction
  def spawn(offset: XY, params: (String,Any)*) : BotAction
  def set(params: (String,Any)*) : BotAction
  def log(text: String) : BotAction
}

trait MiniBotAction extends BotAction {
  // inputs
  def offsetToMaster: XY

  // outputs
  def explode(blastRadius: Int) : BotAction
}


case class BotActionActionImpl(command: CommandFromServer) extends MiniBotAction {
  // input
  val view = command.view
  val energy = command.energy
  val time = command.time
  val generation = command.generation
  def offsetToMaster = command.offsetToMaster.getOrElse(XY.Zero)
  val lastDirection = command.lastDirection


  // output

  private var stateParams = Map.empty[String,Any]     // holds "Set()" commands
  private var commands = ""                           // holds all other commands
  private var debugOutput = ""                        // holds all "Log()" output

  /** Appends a new command to the command string; returns 'this' for fluent API. */
  private def append(s: String) : BotAction = { commands += (if(commands.isEmpty) s else "|" + s); this }

  /** Renders commands and stateParams into a control function return string. */
  override def toString = {
    var result = commands
    if(stateParams.nonEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
    }
    if(!debugOutput.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String) = { debugOutput += text + "\n"; this }
  def move(direction: XY) = append("Move(direction=" + direction + ")")
  def say(text: String) = append("Say(text=" + text + ")")
  def status(text: String) = append("Status(text=" + text + ")")
  def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")
  def spawn(offset: XY, params: (String,Any)*) =
    append("Spawn(direction=" + offset +
      (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")
  def set(params: (String,Any)*) = { stateParams ++= params; this }
  def set(keyPrefix: String, xy: XY) = { stateParams ++= List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y); this }
}


/** Utility class for managing 2D cell coordinates.
  * The coordinate (0,0) corresponds to the top-left corner of the arena on screen.
  * The direction (1,-1) points right and up.
  */
case class XY(x: Int, y: Int) {
  override def toString = x + ":" + y

  def isNonZero = x != 0 || y != 0
  def isZero = x == 0 && y == 0
  def isNonNegative = x >= 0 && y >= 0

  def updateX(newX: Int) = XY(newX, y)
  def updateY(newY: Int) = XY(x, newY)

  def addToX(dx: Int) = XY(x + dx, y)
  def addToY(dy: Int) = XY(x, y + dy)

  def +(pos: XY) = XY(x + pos.x, y + pos.y)
  def -(pos: XY) = XY(x - pos.x, y - pos.y)
  def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

  def distanceTo(pos: XY): Double = (this - pos).length // Phythagorean
  def length: Double = math.sqrt(x * x + y * y) // Phythagorean

  def stepsTo(pos: XY): Int = (this - pos).stepCount // steps to reach pos: max delta X or Y
  def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

  def signum = XY(x.signum, y.signum)

  def negate = XY(-x, -y)
  def negateX = XY(-x, y)
  def negateY = XY(x, -y)

  /** Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
  def toDirection45: Int = {
    val unit = signum
    unit.x match {
      case -1 =>
        unit.y match {
          case -1 =>
            if(x < y * 3) Direction45.Left
            else if(y < x * 3) Direction45.Up
            else Direction45.UpLeft
          case 0 =>
            Direction45.Left
          case 1 =>
            if(-x > y * 3) Direction45.Left
            else if(y > -x * 3) Direction45.Down
            else Direction45.LeftDown
        }
      case 0 =>
        unit.y match {
          case 1 => Direction45.Down
          case 0 => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
          case -1 => Direction45.Up
        }
      case 1 =>
        unit.y match {
          case -1 =>
            if(x > -y * 3) Direction45.Right
            else if(-y > x * 3) Direction45.Up
            else Direction45.RightUp
          case 0 =>
            Direction45.Right
          case 1 =>
            if(x > y * 3) Direction45.Right
            else if(y > x * 3) Direction45.Down
            else Direction45.DownRight
        }
    }
  }

  def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)
  def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)
  def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)
  def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)


  def wrap(boardSize: XY) = {
    val fixedX = if(x < 0) boardSize.x + x else if(x >= boardSize.x) x - boardSize.x else x
    val fixedY = if(y < 0) boardSize.y + y else if(y >= boardSize.y) y - boardSize.y else y
    if(fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
  }
}


object XY {
  /** Parse an XY value from XY.toString format, e.g. "2:3". */
  def apply(s: String) : XY = { val a = s.split(':'); XY(a(0).toInt,a(1).toInt) }

  val Zero = XY(0, 0)
  val One = XY(1, 1)

  val Right     = XY( 1,  0)
  val RightUp   = XY( 1, -1)
  val Up        = XY( 0, -1)
  val UpLeft    = XY(-1, -1)
  val Left      = XY(-1,  0)
  val LeftDown  = XY(-1,  1)
  val Down      = XY( 0,  1)
  val DownRight = XY( 1,  1)

  def fromDirection45(index: Int): XY = index match {
    case Direction45.Right => Right
    case Direction45.RightUp => RightUp
    case Direction45.Up => Up
    case Direction45.UpLeft => UpLeft
    case Direction45.Left => Left
    case Direction45.LeftDown => LeftDown
    case Direction45.Down => Down
    case Direction45.DownRight => DownRight
  }

  def fromDirection90(index: Int): XY = index match {
    case Direction90.Right => Right
    case Direction90.Up => Up
    case Direction90.Left => Left
    case Direction90.Down => Down
  }

  def apply(array: Array[Int]): XY = XY(array(0), array(1))


  def option(s: Option[String]): Option[XY] = {
    s match {
      case Some(direction) => Option(apply(direction))
      case None => Option.empty
    }
  }
}


object Direction45 {
  val Right = 0
  val RightUp = 1
  val Up = 2
  val UpLeft = 3
  val Left = 4
  val LeftDown = 5
  val Down = 6
  val DownRight = 7
}


object Direction90 {
  val Right = 0
  val Up = 1
  val Left = 2
  val Down = 3
}


object Cell extends Enumeration {

  case class CellSymbol(symbol: Char)

  val EMPTY = CellSymbol('_')
  val WITH_WALL = CellSymbol('W')
  val WITH_MY_BOT = CellSymbol('M')
  val WITH_MY_MINI_BOT = CellSymbol('S')
  val WITH_ENEMY_BOT = CellSymbol('m')
  val WITH_ENEMY_MINI_BOT = CellSymbol('s')
  val WITH_EDIBLE_PLANT = CellSymbol('P')
  val WITH_EDIBLE_BEAST = CellSymbol('B')
  val WITH_POISONOUS_PLANT = CellSymbol('p')
  val WITH_PREDATOR_BEAST = CellSymbol('b')
}


case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
  def absPosFromIndex(index: Int) = XY(index % size, index / size)
  def absPosFromRelPos(relPos: XY) = relPos + center
  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
  def relPosFromAbsPos(absPos: XY) = absPos - center
  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  def offsetToNearest(c: Char): Option[XY] = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if( matchingXY.isEmpty )
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      Some(nearest)
    }
  }

  def offsetToNearest(cell: Cell.CellSymbol): Option[XY] = {
    offsetToNearest(cell.symbol)
  }
}


class CommandFromServer(val generation: Int,
                        val name: String,
                        val time: Int,
                        viewAsCharsSquareRegion: String,
                        val energy: Int,
                        master: Option[String],
                        collision: Option[String],
                         lastDir: Option[String]) {

  val offsetToMaster = XY.option(master)
  val collisionDirection = XY.option(collision)
  val view = View(viewAsCharsSquareRegion)
  val lastDirection = lastDir.map(_.toInt).getOrElse(0)

  def isForMasterBot: Boolean = generation == 0
}


object CommandFromServer {

  def apply(command: String): Option[CommandFromServer] = {
    val segments = command.split('(')

    if (segments.length != 2) throw new IllegalStateException("invalid command: " + command)

    if (segments(0) != "React") return Option.empty

    def splitParam(param: String) = {
      val segments = param.split('=')
      if (segments.length != 2)
        throw new IllegalStateException("invalid key/value pair: " + param)
      (segments(0), segments(1))
    }

    val params = segments(1).dropRight(1).split(',').map(splitParam).toMap


    Option(new CommandFromServer(
      params("generation").toInt,
      params("name"),
      params("time").toInt,
      params("view"),
      params("energy").toInt,
      params get "master",
      params get "collision",
      params get "lastDirection"))
  }
}


