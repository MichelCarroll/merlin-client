
import game.GameLoop

import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.annotation.JSExport


@JSExport
object Game {

  @JSExport
  def main(canvas: html.Canvas): Unit = {
    GameLoop.run(canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D])
  }
}