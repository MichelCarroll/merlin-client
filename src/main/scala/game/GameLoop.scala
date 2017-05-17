package game

import scala.util.Random
import com.softwaremill.quicklens._

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.Color
import scala.scalajs.js.timers._

import network.MultiplayerRoom



sealed trait Enemy {
  val coordinate: Coordinate
}
case class Slime(coordinate: Coordinate) extends Enemy

case class Coordinate(x: Int, y: Int)

case class PlayerState(coordinate: Coordinate, color: Color) {

  def execute(command: PlayerCommand): PlayerState = command match {
    case Move(newCoordinate) => copy(coordinate = newCoordinate)
  }

}

sealed trait PlayerCommand
case class Move(coordinate: Coordinate) extends PlayerCommand

sealed trait RealTimeMessage
case class InitialState(playerState: PlayerState) extends RealTimeMessage
case class ExecutePlayerCommand(command: PlayerCommand) extends RealTimeMessage
case class CurrentWorldState(worldState: WorldState) extends RealTimeMessage

case class Teammate(sendMessage: (RealTimeMessage) => Unit, state: PlayerState)

case class PlayerId(id: String) extends AnyVal

case class WorldState(enemies: Set[Enemy])

case class GameState(isHost: Boolean, teammates: Map[PlayerId, Teammate], worldState: WorldState) {


  def execute(command: PlayerCommand, playerId: PlayerId): GameState = command match {
    case Move(newCoordinate) =>
      this
        .modify(_.teammates.at(playerId).state.coordinate)
        .setTo(newCoordinate)
  }

}

object GameLoop {

  def run(drawingContext: dom.CanvasRenderingContext2D): Unit = {

    val id = (Math.random() * 1000).toInt.toString

    val width = 800
    val height = 600

    val rng = new Random(System.currentTimeMillis())

    def randomPositionOnCanvas = Coordinate(rng.nextInt(width), rng.nextInt(height))

    drawingContext.canvas.width = width
    drawingContext.canvas.height = height

    def randomColor: Color = {
      val r = rng.nextInt(255)
      val g = rng.nextInt(255)
      val b = rng.nextInt(255)
      Color(s"rgb($r, $g, $b)")
    }

    var playerState = PlayerState(Coordinate(width / 2, height / 2), randomColor)
    var gameState = GameState(
      isHost = false,
      teammates = Map[PlayerId, Teammate](),
      worldState = WorldState(Set[Enemy]())
    )

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
              gameState = gameState.modify(_.teammates).using(_ + (PlayerId(userId) -> Teammate(sendMessage, state)))

            case ExecutePlayerCommand(command) =>
              gameState.teammates.get(PlayerId(userId)) match {
                case Some(teammate) =>
                  gameState = gameState
                    .modify(_.teammates.at(PlayerId(userId)).state)
                    .using(_.execute(command))
                case _ =>
              }

            case CurrentWorldState(worldState) =>
              gameState = gameState.copy(worldState = worldState)
          }
        }

      },
      onParticipantLeave = (userId) => {
        gameState = gameState.modify(_.teammates).using(_ - PlayerId(userId))
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
      gameState.worldState.enemies.foreach {
        case Slime(coordinate) => drawSquare(Color.Red, coordinate)
      }
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

    def executeCommand(command: PlayerCommand): Unit = {
      playerState = playerState.execute(command)
      gameState.teammates.values.foreach(_.sendMessage(ExecutePlayerCommand(command)))
    }

    setInterval(1000 / 60) { //60 fps
      redraw()
    }

    setInterval(5000) { //every 5 s
      if(gameState.isHost)
        gameState = gameState.modify(_.worldState.enemies).using(_ + Slime(randomPositionOnCanvas))
    }

    setInterval(1000 / 4) { // 4/s
      if(gameState.isHost)
        gameState.teammates.values.foreach(_.sendMessage(CurrentWorldState(gameState.worldState)))
    }
  }
}