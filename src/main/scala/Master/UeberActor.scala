package Master

import GamesBuilder.GamesBuilder.TeamsToMatches
import akka.actor.{Actor, Props}

/**
  * This actor gets a Set of Teams and returns a Set of Games
  */
object UeberActor{
  val name = "ueber-actor"
  val props = Props(new UeberActor)

}

class UeberActor extends Actor{
  val gamesBuilder = context.actorOf(GamesBuilder.GamesBuilder.props)

  var tMode = new TournamentMode()

  def receive: Receive = {
    case TeamsToMatches(teams,mode) =>
      tMode = mode
      gamesBuilder ! TeamsToMatches(teams,mode)
  }


}