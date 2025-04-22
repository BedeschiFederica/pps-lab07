package ex2

import util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot)(val turnBatteryDec: Int, val actBatteryDec: Int) extends Robot:
  require(turnBatteryDec > 0)
  require(actBatteryDec > 0)
  var battery: Int = 100
  export robot.{position, direction}
  private def performAction(action: => Unit, batteryDec: Int): Unit =
    if battery > 0 then action
    battery -= batteryDec
  override def turn(dir: Direction): Unit = performAction(robot.turn(dir), turnBatteryDec)
  override def act(): Unit = performAction(robot.act(), actBatteryDec)

class RobotCanFail(val robot: Robot)(val failureProb: Double) extends Robot:
  require(failureProb >= 0.0 && failureProb <= 1.0)
  export robot.{position, direction}
  private def performAction(action: => Unit): Unit =
    if Random.nextDouble() > failureProb then action
  override def turn(dir: Direction): Unit = performAction(robot.turn(dir))
  override def act(): Unit = performAction(robot.act())

class RobotRepeated(val robot: Robot)(val n: Int) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit = for i <- 1 to n do robot.act()

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
