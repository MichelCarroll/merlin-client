
import java.util.UUID

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.Color

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.timers._

import scala.util.Random


case class Coordinate(x: Int, y: Int)

@JSExport
object Game {

  @JSExport
  def main(canvas: html.Canvas): Unit = {

    val drawingContext = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val id = (Math.random() * 1000).toInt.toString

    val width = 800
    val height = 600

    drawingContext.canvas.width = width
    drawingContext.canvas.height = height

    val rng = new Random(System.currentTimeMillis())
    val r = rng.nextInt(255)
    val g = rng.nextInt(255)
    val b = rng.nextInt(255)
    val randomColor = Color(s"rgb($r, $g, $b)")
    var playerState = PlayerState(Coordinate(width / 2, height / 2), randomColor)

    val multiplayerRoom = new MultiplayerRoom(
      messageOnNewConnection = InitialState(playerState),
      updateState = (message: RealTimeMessage, lastStateOpt: Option[PlayerState]) => message match {

        case InitialState(state) => Some(state)

        case UpdateCoordinate(coordinate) =>
          lastStateOpt match {
            case Some(lastState) =>
              Some(lastState.copy(coordinate = coordinate))
            case _ =>
              None
          }
      }
    )

    var mouseWithinCanvas = false
    var pressingDown = false

    def redraw(): Unit = {

      def drawSquare(color: Color, coordinate: Coordinate): Unit = {
        val squareWidth, squareHeight = 50

        drawingContext.fillStyle = color.toString()
        drawingContext.fillRect(
          coordinate.x - squareWidth / 2,
          coordinate.y - squareHeight / 2,
          squareWidth,
          squareHeight
        )
      }

      drawingContext.fillStyle = "black"
      drawingContext.fillRect(0, 0, width, height)

      drawSquare(playerState.color, playerState.coordinate)
      multiplayerRoom.otherPlayers.values.flatMap(_.stateOpt).foreach(state => drawSquare(state.color, state.coordinate))
    }

    canvas.onmousedown = { event: MouseEvent => pressingDown = true }
    canvas.onmouseup = { event: MouseEvent => pressingDown = false }

    canvas.onmouseenter = { event: MouseEvent => mouseWithinCanvas = true }
    canvas.onmouseout = { event: MouseEvent => mouseWithinCanvas = false }

    canvas.onmousemove = { event: MouseEvent =>
      if(pressingDown && mouseWithinCanvas) {
        playerState = playerState.copy(coordinate = Coordinate(event.clientX.toInt, event.clientY.toInt))
      }
    }

    setInterval(1000 / 60) {
      redraw()
    }

    setInterval(1000 / 30) {
      multiplayerRoom.otherPlayers.foreach {
        case (userId, player) => multiplayerRoom.sendMessage(player.dataChannel, UpdateCoordinate(playerState.coordinate))
      }
    }
  }
}