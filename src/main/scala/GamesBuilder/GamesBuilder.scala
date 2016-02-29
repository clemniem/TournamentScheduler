package GamesBuilder

import GamesBuilder.{GameRounds, TeamsToMatches}
import Master.Types.{Rounds}
import Master.{GameMode, TournamentMode, Team}
import akka.actor.{Actor, Props}


/**
  * This actor gets a Set of Teams and returns a Set of Games
  */
object GamesBuilder {
  val name = "games-builder"
  val props = Props(new GamesBuilder)

  case class TeamsToMatches(teams: List[Team], modus: TournamentMode)
  case class GameRounds(rounds: Rounds)

}

class GamesBuilder extends Actor with RoundRobin {

  def receive: Receive = {
    case TeamsToMatches(teams, mode) => mode.gameMode match {
      case GameMode.RoundRobin => sender ! GameRounds(roundRobin(teams))
      //todo implement pools and elimination
      case _ => sender ! GameRounds(roundRobin(teams))
    }

  }


}

trait RoundRobin {
  def roundRobin(teamsOrig: List[Team]): Rounds = {
    var games: List[(Team, Team)] = Nil
    var teams = teamsOrig
    if (teams.length % 2 == 0) {
      for (t <- teams.indices.drop(1)) {
        games ::=(teams.head, teams(1))
        for (i <- 2 to teams.length / 2) {
          games ::=(teams(i), teams(teams.length + 1 - i))
        }
        teams = teams.head +: rotateLeft(teams.tail,1)
      }
      games.sliding(teamsOrig.size/2,teamsOrig.size/2).toList
    } else {
      for (t <- teams.indices) {
        for (i <- 1 to teams.length / 2){
          games ::= (teams(i), teams(teams.length - i))
        }
        teams = rotateLeft(teams,1)
      }
      games.sliding(teamsOrig.size/2,teamsOrig.size/2).toList
    }
  }

  //todo implement nice with good algorithm
  def rotateLeft[A](seq: List[A], i: Int): List[A] = {
    val size = seq.size
    seq.drop(i % size) ++ seq.take(i % size)
  }
}


//todo implement Elimination-Mode-trait
//todo implement Pools-Mode-trait