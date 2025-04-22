package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatteryTest extends AnyFlatSpec with Matchers:
  "A RobotWithBattery" should "not turn or act when out of battery" in :
    val batteryDecrement = 1
    val robot = RobotWithBattery(new SimpleRobot((0, 0), Direction.North))(batteryDecrement)
    val nActions = math.ceil(100.0 / batteryDecrement / 2).toInt
    for i <- 1 to nActions do robot.turn(Direction.East)
    for i <- 1 to nActions do robot.act()
    robot.direction should be(Direction.East)
    robot.position should be((nActions, 0))

    robot.turn(Direction.South)
    robot.direction should be(Direction.East)

    robot.act()
    robot.position should be((nActions, 0))
