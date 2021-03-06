package network

import java.util.UUID

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.experimental.webrtc._
import upickle.default._

import shared.SocketMessage
import shared.SocketMessage._

case class PendingConnection(peerConnection: RTCPeerConnection)
case class Participant(dataChannel: RTCDataChannel)

class MultiplayerRoom(onParticipantEnter: (String, Participant) => Unit, onParticipantLeave: (String) => Unit, onBecomeHost: () => Unit) {

  import scala.concurrent.ExecutionContext.Implicits.global

  var ws = new dom.WebSocket(s"ws://ec2-54-213-211-245.us-west-2.compute.amazonaws.com:8080")
  var pendingConnections = Map[String, PendingConnection]()
  var otherParticipants = Map[String, Participant]()

  def disconnect(userId: String): Unit = {
    pendingConnections -= userId
    otherParticipants -= userId
  }

  def createPeerConnection(userId: String): RTCPeerConnection = {

    val peerConnection = new RTCPeerConnection(RTCConfiguration(
      iceServers = scalajs.js.Array[RTCIceServer](RTCIceServer(urls = "stun:stun.l.google.com:19302"))
    ))

    peerConnection.onicecandidate = { event: RTCPeerConnectionIceEvent =>
      if(event.candidate != null) {
        println("SENT CANDIDATE")
        ws.send(write(IceCandidateOutgoing(userId, event.candidate.candidate, event.candidate.sdpMLineIndex)))
      }
    }

    peerConnection.oniceconnectionstatechange = { event: Event =>
      peerConnection.iceConnectionState match {
        case RTCIceConnectionState.closed => disconnect(userId)
        case RTCIceConnectionState.failed => disconnect(userId)
        case RTCIceConnectionState.disconnected => disconnect(userId)
        case _ =>
      }
    }

    peerConnection
  }

  def handleDataChannel(userId: String, dataChannel: RTCDataChannel): Unit = {

    println(s"ondata channel ${dataChannel.label}")

    dataChannel.onclose = { event: Event =>
      println("CHANNEL CLOSED....")
      otherParticipants -= userId
      onParticipantLeave(userId)
    }

    dataChannel.onerror = { event: Event =>
      println("CHANNEL ERRORED....")
      otherParticipants -= userId
      onParticipantLeave(userId)
    }

    dataChannel.onopen = { event: Event =>
      println("CHANNEL OPENED...")
      pendingConnections -= userId
      val participant = Participant(dataChannel)
      otherParticipants += userId -> participant
      onParticipantEnter(userId, participant)
    }

  }

  def createPendingCallee(userId: String): PendingConnection = {

    val peerConnection = createPeerConnection(userId)

    val channelID = UUID.randomUUID().toString
    println(s"creating data channel $channelID")
    val channel = peerConnection.createDataChannel(channelID, RTCDataChannelInit(ordered = false))
    handleDataChannel(userId, channel)
    PendingConnection(peerConnection)
  }

  def createPendingCaller(userId: String): PendingConnection = {

    val peerConnection = createPeerConnection(userId)

    peerConnection.ondatachannel = { event: RTCDataChannelEvent =>
      handleDataChannel(userId, event.channel)
    }

    PendingConnection(peerConnection)
  }

  ws.onopen = { x: Event =>

    import scala.scalajs.js.Thenable.Implicits._

    ws.onmessage = { event: MessageEvent =>
      read[SocketMessage](event.data.toString) match {

        case OfferRequest(userId) =>
          println("SENT OFFER REQUEST" )
          val pendingCallee = createPendingCallee(userId)
          pendingConnections += userId -> pendingCallee

          pendingCallee.peerConnection.createOffer().onSuccess {
            case (localDescription) => {
              println("SENT OFFER")
              pendingCallee.peerConnection.setLocalDescription(localDescription)
              ws.send(write(Offer(userId, localDescription.`type`.toString, localDescription.sdp)))
            }
          }

        case Offer(userId, t, s) =>
          println("GOT OFFER")

          val pendingCaller = createPendingCaller(userId)
          pendingConnections += userId -> pendingCaller
          pendingCaller.peerConnection.setRemoteDescription(new RTCSessionDescription(RTCSessionDescriptionInit(t.asInstanceOf[RTCSdpType], s)))

          pendingCaller.peerConnection.createAnswer().onSuccess {
            case (answer) => {
              println("SENT ANSWER")
              pendingCaller.peerConnection.setLocalDescription(answer)
              ws.send(write(Answer(userId, answer.`type`.toString, answer.sdp)))
            }
          }

        case Answer(userId, t, s) =>
          println("GOT ANSWER")
          pendingConnections.get(userId) match {
            case Some(pendingConnection) => pendingConnection.peerConnection.setRemoteDescription(
              new RTCSessionDescription(RTCSessionDescriptionInit(t.asInstanceOf[RTCSdpType], s))
            )
            case _ =>
          }

        case IceCandidateIncoming(userId, candidate, sdpMLineIndex) =>
          println("GOT CANDIDATE")
          pendingConnections.get(userId) match {
            case Some(pendingConnection) =>
              pendingConnection.peerConnection.addIceCandidate(new RTCIceCandidate(RTCIceCandidateInit(
                candidate = candidate,
                sdpMLineIndex = sdpMLineIndex
              )))
            case _ =>
          }

        case IceCandidateOutgoing(userId, candidate, sdpMLineIndex) =>
          println("SHOULD NEVER HAPPEN")

        case BecomeHost(_) =>
          onBecomeHost()
      }
    }


  }

  ws.onerror = { x: ErrorEvent =>
    println("some error has occured " + x.message)
  }

  ws.onclose = { x: CloseEvent =>
    println("WS connection CLOSED !!")
  }


}