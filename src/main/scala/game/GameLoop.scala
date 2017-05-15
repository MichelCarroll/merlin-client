package game


import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.Color

import scala.scalajs.js.timers._
import scala.util.Random
import upickle.default._

import network.{MultiplayerRoom, Participant}

case class Coordinate(x: Int, y: Int)

case class PlayerState(coordinate: Coordinate, color: Color)

sealed trait RealTimeMessage
case class InitialState(playerState: PlayerState) extends RealTimeMessage
case class UpdateCoordinate(coordinate: Coordinate) extends RealTimeMessage

case class OtherPlayer(participant: Participant, stateOpt: Option[PlayerState])

object GameLoop {

  def run(drawingContext: dom.CanvasRenderingContext2D): Unit = {

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
    var otherPlayers = Map[String, OtherPlayer]()

    val multiplayerRoom = new MultiplayerRoom(
      onParticipantEnter = (userId, participant) => {
        otherPlayers += userId -> OtherPlayer(participant, None)

        participant.dataChannel.send(write[RealTimeMessage](InitialState(playerState)))

        participant.dataChannel.onmessage = { event: MessageEvent =>
          val message = read[RealTimeMessage](event.data.toString)
          val newState = message match {
            case InitialState(state) => Some(state)

            case UpdateCoordinate(coordinate) =>
              otherPlayers(userId).stateOpt match {
                case Some(lastState) =>
                  Some(lastState.copy(coordinate = coordinate))
                case _ =>
                  None
              }
          }
          otherPlayers = otherPlayers.updated(userId, OtherPlayer(participant, newState))
        }

      },
      onParticipantLeave = (userId) => {
        otherPlayers -= userId
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
      otherPlayers.values.flatMap(_.stateOpt).foreach(state => drawSquare(state.color, state.coordinate))
    }

    drawingContext.canvas.onmousedown = { event: MouseEvent => pressingDown = true }
    drawingContext.canvas.onmouseup = { event: MouseEvent => pressingDown = false }

    drawingContext.canvas.onmouseenter = { event: MouseEvent => mouseWithinCanvas = true }
    drawingContext.canvas.onmouseout = { event: MouseEvent => mouseWithinCanvas = false }

    drawingContext.canvas.onmousemove = { event: MouseEvent =>
      if(pressingDown && mouseWithinCanvas) {
        playerState = playerState.copy(coordinate = Coordinate(event.clientX.toInt, event.clientY.toInt))
      }
    }

    setInterval(1000 / 60) {
      redraw()
    }

    setInterval(1000 / 30) {
      otherPlayers.foreach {
        case (userId, player) => player.participant.dataChannel.send(write[RealTimeMessage](UpdateCoordinate(playerState.coordinate)))
      }
    }
  }
}