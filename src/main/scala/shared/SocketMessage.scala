package shared

sealed trait SocketMessage
object SocketMessage {

  import upickle.default._
  implicit val readWriter: ReadWriter[SocketMessage] =
    macroRW[OfferRequest] merge macroRW[Offer] merge macroRW[Answer] merge macroRW[IceCandidateOutgoing]  merge macroRW[IceCandidateIncoming]

  case class OfferRequest(userId: String) extends SocketMessage
  case class Offer(userId: String, `type`: String, sdp: String) extends SocketMessage
  case class Answer(userId: String, `type`: String, sdp: String) extends SocketMessage
  case class IceCandidateOutgoing(userId: String, candidate: String, sdpMLineIndex: Double) extends SocketMessage
  case class IceCandidateIncoming(userId: String, candidate: String, sdpMLineIndex: Double) extends SocketMessage
}