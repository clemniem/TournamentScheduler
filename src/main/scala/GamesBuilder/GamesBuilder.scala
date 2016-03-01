package GamesBuilder

import GamesBuilder.{GameRounds, TeamsToMatches}
import Master.Types.{Round}
import Master.{GameMode, TournamentMode, Team}
import akka.actor.{Actor, Props}


/**
  * This actor gets a Set of Teams and returns a Set of Games
  */
object GamesBuilder {
  val name = "games-builder"
  val props = Props(new GamesBuilder)

  case class TeamsToMatches(teams: List[Team], modus: TournamentMode)
  case class GameRounds(rounds: List[Round])

}

class GamesBuilder extends Actor with RoundRobin with Elimination {

  def receive: Receive = {
    case TeamsToMatches(teams, mode) => mode.gameMode match {
      case GameMode.RoundRobin  => sender ! GameRounds(roundsForRoundRobin(teams))
      case GameMode.Elimination => sender ! GameRounds(roundsForElimination(teams))
      //todo implement pools and elimination
      case _ => sender ! GameRounds(roundsForRoundRobin(teams))
    }

  }


}

trait RoundRobin {
  def roundsForRoundRobin(teamsOrig: List[Team]): List[Round] = {
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
      (Stream.from(1)).zip(games).toList.sliding(teamsOrig.size/2,teamsOrig.size/2).toList
    } else {
      for (t <- teams.indices) {
        for (i <- 1 to teams.length / 2){
          games ::= (teams(i), teams(teams.length - i))
        }
        teams = rotateLeft(teams,1)
      }
      Stream.from(1).zip(games).toList.sliding(teamsOrig.size/2,teamsOrig.size/2).toList
    }
  }

  //todo implement nice with good algorithm
  def rotateLeft[A](seq: List[A], i: Int): List[A] = {
    val size = seq.size
    seq.drop(i % size) ++ seq.take(i % size)
  }
}

trait Elimination {
  def roundsForElimination(teams:List[Team]): List[Round] = {
    eliminationRecursive(seedForElimination(teams)) //todo implement
  }

  def seedForElimination(teams: List[Team]): List[Round] = {
    val sortedTeams = teams.sortBy(_.MeanStrength).reverse
    val flippedLowerHalf = sortedTeams.drop(teams.size/2).reverse
    val games = Stream.from(1).zip(flippedLowerHalf.zip(sortedTeams)).toList

  }


  def eliminationRecursive(rounds:List[Round]): List[Round] = rounds match {
    case seed :: Nil => Nil //create first W and L bracket
    case wBracket :: lBracket :: seed :: Nil => Nil //create next Brackets
    case wBracket :: lBracket :: pBracket :: rs => Nil // create next Brackets
    case _ => Nil //ERROR
  }


}


//todo implement Elimination-Mode-trait
//todo implement Pools-Mode-trait