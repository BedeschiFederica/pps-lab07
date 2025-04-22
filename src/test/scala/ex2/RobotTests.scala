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

class RobotCanFailTest extends AnyFlatSpec with Matchers:
  private def testRobot(failureProb: Double): Robot =
    val robot = RobotCanFail(new SimpleRobot((0, 0), Direction.North))(failureProb)
    for i <- 1 to 1000 do robot.turn(Direction.East)
    for i <- 1 to 1000 do robot.act()
    robot

  "A RobotCanFail" should "always fail with failure probability = 1" in :
    val robot = testRobot(1)
    robot.direction should be(Direction.North)
    robot.position should be((0, 0))

  it should "never fail with failure probability = 0" in :
    val robot = testRobot(0)
    robot.direction should be(Direction.East)
    robot.position should be((1000, 0))

  it should "sometimes fail with failure probability = 0.5" in :
    val robot = testRobot(0.5)
    robot.direction should be(Direction.East)
    robot.position should not be((1000, 0))

class RobotRepeatedTest extends AnyFlatSpec with Matchers:
  "A RobotRepeated" should "perform actions multiple times" in :
    val n = 5
    val robot = RobotRepeated(new SimpleRobot((0, 0), Direction.North))(n)
    robot.turn(Direction.East)
    for i <- 1 to 10 do robot.act()
    robot.direction should be(Direction.East)
    robot.position should be((10 * n, 0))