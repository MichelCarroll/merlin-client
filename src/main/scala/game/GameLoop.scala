package game


import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.Color

import scala.scalajs.js.timers._
import scala.util.Random

import network.MultiplayerRoom


case class Coordinate(x: Int, y: Int)

case class PlayerState(coordinate: Coordinate, color: Color) {

  def execute(command: GameCommand): PlayerState = command match {
    case Move(newCoordinate) => copy(coordinate = newCoordinate)
  }

}

sealed trait GameCommand
case class Move(coordinate: Coordinate) extends GameCommand

sealed trait RealTimeMessage
case class InitialState(playerState: PlayerState) extends RealTimeMessage
case class ExecuteCommand(command: GameCommand) extends RealTimeMessage

case class Teammate(sendMessage: (RealTimeMessage) => Unit, state: PlayerState)

case class GameState(isHost: Boolean, teammates: Map[String, Teammate])

object GameLoop {

  def run(drawingContext: dom.CanvasRenderingContext2D): Unit = {

    val id = (Math.random() * 1000).toInt.toString

    val width = 800
    val height = 600

    drawingContext.canvas.width = width
    drawingContext.canvas.height = height

    def randomColor: Color = {
      val rng = new Random(System.currentTimeMillis())
      val r = rng.nextInt(255)
      val g = rng.nextInt(255)
      val b = rng.nextInt(255)
      Color(s"rgb($r, $g, $b)")
    }

    var playerState = PlayerState(Coordinate(width / 2, height / 2), randomColor)
    var gameState = GameState(isHost = false, teammates = Map[String, Teammate]())

    val multiplayerRoom = new MultiplayerRoom(
      onParticipantEnter = (userId, participant) => {

        import upickle.default.{ read, write }

        val sendMessage = { message: RealTimeMessage =>
          participant.dataChannel.send(write[RealTimeMessage](message))
        }
        sendMessage(InitialState(playerState))

        participant.dataChannel.onmessage = { event: MessageEvent =>
          read[RealTimeMessage](event.data.toString) match {

            case InitialState(state) =>
              gameState = gameState.copy(teammates = gameState.teammates + (userId -> Teammate(sendMessage, state)))

            case ExecuteCommand(command) =>
              gameState.teammates.get(userId) match {
                case Some(teammate) =>
                  gameState = gameState.copy(teammates =
                    gameState.teammates.updated(userId,
                      teammate.copy(state = teammate.state.execute(command))))
                case _ =>
              }
          }
        }

      },
      onParticipantLeave = (userId) => {
        gameState = gameState.copy(teammates = gameState.teammates - userId)
      },
      onBecomeHost = () => {
        gameState = gameState.copy(isHost = true)
      }
    )


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
      gameState.teammates.values.map(_.state).foreach(state => drawSquare(state.color, state.coordinate))
    }

    var mouseWithinCanvas = false
    var pressingDown = false

    drawingContext.canvas.onmousedown = { event: MouseEvent => pressingDown = true }
    drawingContext.canvas.onmouseup = { event: MouseEvent => pressingDown = false }

    drawingContext.canvas.onmouseenter = { event: MouseEvent => mouseWithinCanvas = true }
    drawingContext.canvas.onmouseout = { event: MouseEvent => mouseWithinCanvas = false }

    drawingContext.canvas.onmousemove = { event: MouseEvent =>
      if(pressingDown && mouseWithinCanvas) {
        executeCommand(Move(Coordinate(event.clientX.toInt, event.clientY.toInt)))
      }
    }

    def executeCommand(command: GameCommand): Unit = {
      playerState = playerState.execute(command)
      gameState.teammates.values.foreach(_.sendMessage(ExecuteCommand(command)))
    }

    setInterval(1000 / 60) {
      redraw()
    }
  }
}