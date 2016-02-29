package Master

import GamesBuilder.GamesBuilder.{GameRounds, TeamsToMatches}
import Master.Types.Rounds
import Master.UeberActor.{FinishedSchedule, MakeSchedule}
import akka.actor.{ActorRef, Actor, Props}

/**
  * This is the UeberActor of the TournamentSchedule MicroService
  */
object UeberActor{
  val name = "ueber-actor"
  def props = Props(new UeberActor)

  case class MakeSchedule(rounds:Rounds,mode: TournamentMode)
  case class FinishedSchedule(slots: List[String])
}

class UeberActor extends Actor{
  val gamesBuilder = context.actorOf(GamesBuilder.GamesBuilder.props)
  val scheduler = context.actorOf(Scheduler.Scheduler.props)
  var realSender: ActorRef = null

  var tMode = new TournamentMode()

  def receive: Receive = {
    case TeamsToMatches(teams,mode) =>
      realSender = sender
      tMode = mode
      gamesBuilder ! TeamsToMatches(teams,mode)
    case GameRounds(rounds) => scheduler ! MakeSchedule(rounds,tMode)
    case FinishedSchedule(slots) => realSender ! FinishedSchedule(slots)
  }


}