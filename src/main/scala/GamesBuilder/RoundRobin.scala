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

  case class TeamsToMatches(teams: Seq[Team], modus: TournamentMode)
  case class Matches(matches: Seq[(Team,Team)])

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
  //todo implement nice with good algorithm
  def roundRobin(teamsOrig: Seq[Team]): Seq[(Team, Team)] = {
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
    } else {
      for (t <- teams.indices.drop(1)) {
        for (i <- 1 to teams.length / 2)
          games ::=(teams(i), teams(teams.length - i))
      }
      teams = rotateLeft(teams,1)
    }
    games
  }

  def rotateLeft[A](seq: Seq[A], i: Int): Seq[A] = {
    val size = seq.size
    seq.drop(i % size) ++ seq.take(i % size)
  }



}
