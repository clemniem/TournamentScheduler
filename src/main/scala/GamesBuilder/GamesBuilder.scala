package GamesBuilder

import GamesBuilder.{Matches, TeamsToMatches}
import Master.{GameMode, TournamentMode, Team}
import akka.actor.{Actor, Props}


/**
  * This actor gets a Set of Teams and returns a Set of Games
  */
object GamesBuilder {
  val name = "games-builder"
  val props = Props(new GamesBuilder)

  case class TeamsToMatches(teams: List[Team], modus: TournamentMode)
  case class Matches(matches: List[(Team,Team)])

}

class GamesBuilder extends Actor with RoundRobin {

  def receive: Receive = {
    case TeamsToMatches(teams, mode) => mode.gameMode match {
      case GameMode.RoundRobin => sender ! Matches(roundRobin(teams))
      //todo implement pools and elimination
      case _ => sender ! Matches(roundRobin(teams))
    }

  }


}

trait RoundRobin {
  def roundRobin(teams: List[Team]): List[(Team, Team)] = {
    //todo sort as rounds of matches (1,2,3,4) -> ((1,2),(3,4)),((1,3),(2,4))...
    for (teamX <- teams; teamY <- teams if teamX.id < teamY.id) yield (teamX,teamY)
  }
}


//todo implement Elimination-Mode-trait
//todo implement Pools-Mode-trait