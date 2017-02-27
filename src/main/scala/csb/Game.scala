package csb

import csb.player.Player
import csb.player.Pod
import csb.player.Angle
import csb.player.Point

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random

object Screen {
    def width = 860
    def height = 484
}

object Board {
    def width = 16000
    def height = 9000
}

object Pixel {
    def fromPoint(p: Point): Pixel =
          Pixel((p.x / Board.width * Screen.width).toInt, (-p.y / Board.height * Screen.height).toInt)
}

case class Pixel(x: Int, y: Int) {
    def toPoint = Point(x * Board.width / Screen.width, - y * Board.width / Screen.width)
}

class Game {

  // 0- set initial position of checkpoints and pods
  val laps = 3
  val checkpoints: Seq[Point] = initCheckpoints
  var pods: Seq[Pod] = initPods

  def initCheckpoints() =
        List(Pixel(304, 138), Pixel(220, 401), Pixel(725, 126), Pixel(696, 390)).map { p => p.toPoint }

  val players = for (i <- 0 to 2) yield Player(checkpoints.toList, laps)

  def step() = {
          val commands = players.flatMap{ p => p.update(pods.toList) }
          pods = updatePods(commands)
  }

  def plot(renderer: dom.CanvasRenderingContext2D) = pods.foreach(plotPod(_, renderer))
 
  // TODO
  def initPods = for (i <- 0 to 2) yield Pod(checkpoints(0), checkpoints(1), Angle(0), Point(0, 0))
  def updatePods(commands: Seq[Tuple2[Point,Double]]): Seq[Pod] = pods
  def plotPod(pod: Pod, renderer: dom.CanvasRenderingContext2D) = {
    renderer.fillStyle = "darkblue"
    val pix = Pixel.fromPoint(pod.position)
    val size = Pixel.fromPoint(Point(pod.podSize, pod.podSize))
    renderer.fillRect(pix.x, pix.y, size.x, size.y)
    println("hello")
  }
}

@JSExport
object Game {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val renderer = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]

    var count = 0

    val game = new Game()

    var p = Point(0, 0)
    val corners = Seq(Point(255, 255), Point(0, 255), Point(128, 0))


    def clear() = {
      renderer.fillStyle = "rgba(255, 255, 255, 0.0)"
      renderer.fillRect(0, 0, Screen.width, Screen.height)
    }

    def run = {
      game.plot(renderer)
      game.step()
    }

    dom.window.setInterval(() => run, 50)
  }
}
