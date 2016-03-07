package Master

import GamesBuilder.GamesBuilder.{GameRounds, TeamsToRounds}
import Master.Types.Round
import Master.UeberActor.{ErrorMessage, FinishedSchedule, MakeSchedule}
import akka.actor.{ActorRef, Actor, Props}

/**
  * This is the UeberActor of the TournamentSchedule MicroService
  */
object UeberActor {
  val name = "ueber-actor"

  def props = Props(new UeberActor)

  case class MakeSchedule(rounds: List[Round], mode: TournamentMode)
  case class FinishedSchedule(slots: List[String])
  case class ErrorMessage(message: String)

}

class UeberActor extends Actor {
  val gamesBuilder = context.actorOf(GamesBuilder.GamesBuilder.props)
  val scheduler = context.actorOf(Scheduler.Scheduler.props)
  var realSender: ActorRef = null

  var tMode = new TournamentMode()

  def receive: Receive = {
    case TeamsToRounds(teams, mode) =>
      realSender = sender
      tMode = mode
      gamesBuilder ! TeamsToRounds(teams, mode)
    case GameRounds(rounds) => rounds match {
      case Nil => realSender ! ErrorMessage("Number of Teams not supported for chosen Game Mode\nElimination only supports (4,8,16)\nPools only supports (8,10,12)!")
      case _ => scheduler ! MakeSchedule(rounds, tMode)
    }
    case FinishedSchedule(slots) => realSender ! FinishedSchedule(slots)
  }


}