package GamesBuilder

import GamesBuilder.{GameRounds, TeamsToRounds}
import Master.Types.{Round}
import Master._
import akka.actor.{Actor, Props}


/**
  * This actor gets a Set of Teams and returns a Set of Games
  */

object GamesBuilder {
  val name = "games-builder"
  val props = Props(new GamesBuilder)

  case class TeamsToRounds(teams: List[Team], modus: TournamentMode)
  case class GameRounds(rounds: List[Round])
}

class GamesBuilder extends Actor with RoundRobin with Elimination with Pools {

  def receive: Receive = {
    case TeamsToRounds(teams, mode) => mode.gameMode match {
      case GameMode.RoundRobin => sender ! GameRounds(roundsForRoundRobin(teams))
      case GameMode.Elimination => sender ! GameRounds(roundsForElimination(teams))
      case GameMode.Pools => sender ! GameRounds(roundsForPools(teams))
    }
  }
}

trait RoundRobin {
  //this algorithm implemented: http://nrich.maths.org/1443
  //returns all pairings for RoundRobin as a List of Rounds for Odd and Even Teams
  def roundsForRoundRobin(teamsOrig: List[Team]): List[Round] = {
    var games: List[(Team, Team)] = Nil
    var teams = teamsOrig
    if (teams.length % 2 == 0) {
      for (t <- teams.indices.drop(1)) {
        games ::=(teams.head, teams(1))
        for (i <- 2 to teams.length / 2) {
          games ::=(teams(i), teams(teams.length + 1 - i))
        }
        teams = teams.head +: rotateLeft(teams.tail, 1)
      }
      Stream.from(1).zip(games).toList.sliding(teamsOrig.size / 2, teamsOrig.size / 2).toList
    } else {
      for (t <- teams.indices) {
        for (i <- 1 to teams.length / 2) {
          games ::=(teams(i), teams(teams.length - i))
        }
        teams = rotateLeft(teams, 1)
      }
      Stream.from(1).zip(games).toList.sliding(teamsOrig.size / 2, teamsOrig.size / 2).toList
    }
  }

  //todo implement maybe more efficiently
  def rotateLeft[A](seq: List[A], i: Int): List[A] = {
    val size = seq.size
    seq.drop(i % size) ++ seq.take(i % size)
  }
}

trait Elimination extends RoundFormatter {
  //hardcoded Rounds for Elimination Tournaments
  def roundsForElimination(teams: List[Team]): List[Round] = teams.size match {
    case 4 =>
      var results: List[Round] = Nil
      val templateRounds4: List[Round] = List(
        List(
          (1, (Team(id = 1), Team(id = 3))),
          (2, (Team(id = 2), Team(id = 4)))
        ),
        List(
          (3, (Team(name = "L1"), Team(name = "L2"))),
          (4, (Team(name = "W1"), Team(name = "W2")))
        )
      )
      val teamNamesByStrength = ("Dummy" :: teams.sortBy(t => t.meanStrength).map(_.name)).toVector
      for (round <- templateRounds4.reverse) {
        results ::= fillRoundsWithTeamNames(teamNamesByStrength, round)
      }
      results

    case 8 =>
      var results: List[Round] = Nil
      val templateRounds4: List[Round] = List(
        List(
          (1, (Team(id = 1), Team(id = 8))),
          (2, (Team(id = 3), Team(id = 6))),
          (3, (Team(id = 2), Team(id = 7))),
          (4, (Team(id = 4), Team(id = 5)))
        ),
        List(
          (5, (Team(name = "L1"), Team(name = "L2"))),
          (6, (Team(name = "L3"), Team(name = "L4"))),
          (7, (Team(name = "W1"), Team(name = "W2"))),
          (8, (Team(name = "W3"), Team(name = "W4")))
        ),
        List(
          (9, (Team(name = "L5"), Team(name = "L6"))),
          (10, (Team(name = "W5"), Team(name = "W6"))),
          (11, (Team(name = "L7"), Team(name = "L8"))),
          (12, (Team(name = "W7"), Team(name = "W8")))
        )
      )
      val teamNamesByStrength = ("Dummy" :: teams.sortBy(t => t.meanStrength).map(_.name)).toVector
      for (round <- templateRounds4.reverse) {
        results ::= fillRoundsWithTeamNames(teamNamesByStrength, round)
      }
      results
    //Todo
    //case 16 => Nil
    case _ =>
      println("Unsupported number of Teams! Supported Teams (4,8)")
      Nil
  }
}

//todo implement Pools-Mode-trait
trait Pools extends RoundFormatter {
  //hardcoded Rounds for Pool-Games Tournament
  def roundsForPools(teams: List[Team]): List[Round] = teams.size match {
    case 8 => Nil
    case 10 => Nil
    case 12 => Nil
    case 16 => Nil
  }
}

trait RoundFormatter {
  def fillRoundsWithTeamNames(teamNames: Vector[String], round: Round): Round = {
    var results: Round = Nil
    for (game@(id, (team1, team2)) <- round.reverse) (team1.id, team2.id) match {
      case (t1, t2) =>
        if (t1 <= 0 && t2 <= 0) results ::= game
        if (t1 <= 0 && t2 > 0) results ::=(id, (team1, team2.copy(name = teamNames(t2))))
        if (t1 > 0 && t2 <= 0) results ::=(id, (team1.copy(name = teamNames(t1)), team2))
        if (t1 > 0 && t2 > 0) results ::=(id, (team1.copy(name = teamNames(t1)), team2.copy(name = teamNames(t2))))
    }
    results
  }
}
