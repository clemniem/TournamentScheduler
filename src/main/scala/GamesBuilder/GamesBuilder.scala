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

trait Elimination extends RoundFormatter with TemplateRounds {
  //hardcoded Rounds for Elimination Tournaments
  def roundsForElimination(teams: List[Team]): List[Round] = {
    var results: List[Round] = Nil

    val teamNamesByStrength = ("Zero" :: teams.sortBy(t => t.meanStrength).map(_.name)).toVector
    for (round <- teams.size match {
      case 4 => eliminationRounds4
      case 8 => eliminationRounds8
      case 16 => eliminationRounds16
      case _ => Nil
    }) {
      results ::= fillRoundWithTeamNames(teamNamesByStrength, round)
    }
    results
  }
}

trait Pools extends RoundFormatter with TemplateRounds{
  def roundsForPools(teams: List[Team]): List[Round] = {
    var results: List[Round] = Nil
    val teamNamesByStrength = ("Zero" :: teams.sortBy(t => t.meanStrength).map(_.name)).toVector
    for (round <- teams.size match {
      case 8 => poolsRounds8
      case 10 => poolsRounds10
      case 12 => poolsRounds12
      case _ => Nil
    }) {
      results ::= fillRoundWithTeamNames(teamNamesByStrength, round)
    }
    results

  }
}

//trait with templateRounds
trait TemplateRounds {
  val eliminationRounds4: List[Round] = List(
    List(
      (1, (Team(id = 1), Team(id = 3))),
      (2, (Team(id = 2), Team(id = 4)))
    ),
    List(
      (3, (Team(name = "L1"), Team(name = "L2"))),
      (4, (Team(name = "W1"), Team(name = "W2")))
    )
  ).reverse

  val eliminationRounds8: List[Round] = List(
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
  ).reverse


  val eliminationRounds16: List[Round] = List(
    List(
      (1, (Team(id = 1), Team(id = 16))),
      (2, (Team(id = 3), Team(id = 14))),
      (3, (Team(id = 5), Team(id = 13))),
      (4, (Team(id = 7), Team(id = 10))),

      (5, (Team(id = 2), Team(id = 15))),
      (6, (Team(id = 4), Team(id = 13))),
      (7, (Team(id = 6), Team(id = 11))),
      (8, (Team(id = 8), Team(id = 9)))
    ),
    List(
      (9, (Team(name = "L1"), Team(name = "L2"))),
      (10, (Team(name = "L3"), Team(name = "L4"))),
      (11, (Team(name = "L5"), Team(name = "L6"))),
      (12, (Team(name = "L7"), Team(name = "L8"))),

      (13, (Team(name = "W1"), Team(name = "W2"))),
      (14, (Team(name = "W3"), Team(name = "W4"))),
      (15, (Team(name = "W5"), Team(name = "W6"))),
      (16, (Team(name = "W7"), Team(name = "W8")))
    ),
    List(
      (17, (Team(name = "L9"), Team(name = "L10"))),
      (18, (Team(name = "L11"), Team(name = "L12"))),
      (19, (Team(name = "W9"), Team(name = "W10"))),
      (20, (Team(name = "W11"), Team(name = "W12"))),

      (21, (Team(name = "L13"), Team(name = "L14"))),
      (22, (Team(name = "L15"), Team(name = "L16"))),

      (23, (Team(name = "W13"), Team(name = "W14"))),
      (24, (Team(name = "W15"), Team(name = "W16")))
    ),
    List(
      (25, (Team(name = "L17"), Team(name = "L18 (*15)"))),
      (26, (Team(name = "W17"), Team(name = "W18 (*13)"))),
      (27, (Team(name = "L19"), Team(name = "L20 (*11)"))),
      (28, (Team(name = "W19"), Team(name = "W20 (* 9)"))),

      (29, (Team(name = "L21"), Team(name = "L22 (* 7)"))),
      (30, (Team(name = "W21"), Team(name = "W22 (* 5)"))),

      (31, (Team(name = "L23"), Team(name = "L24 (* 3)"))),
      (32, (Team(name = "W23"), Team(name = "W24 (* 1)")))
    )
  ).reverse

  val poolsRounds8 = List(
    List(
      (1, (Team(id = 1), Team(id = 7))),
      (2, (Team(id = 5), Team(id = 3))),

      (3, (Team(id = 2), Team(id = 8))),
      (4, (Team(id = 4), Team(id = 6)))
    ),
    List(
      (5, (Team(id = 1), Team(id = 5))),
      (6, (Team(id = 3), Team(id = 7))),

      (7, (Team(id = 2), Team(id = 6))),
      (8, (Team(id = 4), Team(id = 8)))
    ),
    List(
      (9, (Team(id = 1), Team(id = 3))),
      (10, (Team(id = 5), Team(id = 7))),

      (11, (Team(id = 2), Team(id = 4))),
      (12, (Team(id = 6), Team(id = 8)))
    ),
    List(
      (13, (Team(name = "A1"), Team(name = "B4"))),
      (14, (Team(name = "A2"), Team(name = "B3"))),
      (15, (Team(name = "A3"), Team(name = "B2"))),
      (16, (Team(name = "A4"), Team(name = "B1")))
    ),
    List(
      (25, (Team(name = "L13"), Team(name = "L14"))),
      (26, (Team(name = "L15"), Team(name = "L16"))),
      (27, (Team(name = "W13"), Team(name = "W14"))),
      (28, (Team(name = "W15"), Team(name = "W16")))
    ),
    List(
      (29, (Team(name = "L25"), Team(name = "L26 (*7)"))),
      (30, (Team(name = "W25"), Team(name = "W26 (*5)"))),
      (31, (Team(name = "L27"), Team(name = "L28 (*3)"))),
      (32, (Team(name = "W27"), Team(name = "W28 (*1)")))
    )
  ).reverse


  val poolsRounds10 = List(
    List(
      (1, (Team(id = 9), Team(id = 7))),
      (2, (Team(id = 5), Team(id = 3))),

      (3, (Team(id = 10), Team(id = 8))),
      (4, (Team(id = 6), Team(id = 4)))
    ),
    List(
      (5, (Team(id = 1), Team(id = 7))),
      (6, (Team(id = 9), Team(id = 5))),

      (7, (Team(id = 2), Team(id = 8))),
      (8, (Team(id = 10), Team(id = 6)))
    ),
    List(
      (9, (Team(id = 1), Team(id = 3))),
      (10, (Team(id = 5), Team(id = 7))),

      (11, (Team(id = 2), Team(id = 4))),
      (12, (Team(id = 6), Team(id = 8)))
    ),
    List(
      (13, (Team(id = 1), Team(id = 5))),
      (14, (Team(id = 9), Team(id = 3))),

      (15, (Team(id = 2), Team(id = 6))),
      (16, (Team(id = 10), Team(id = 4)))
    ),
    List(
      (17, (Team(id = 1), Team(id = 9))),
      (18, (Team(id = 3), Team(id = 7))),

      (19, (Team(id = 2), Team(id = 10))),
      (20, (Team(id = 4), Team(id = 8)))
    ),
    List(
      (21, (Team(name = "A2"), Team(name = "B3"))),
      (22, (Team(name = "A3"), Team(name = "B2"))),
      (23, (Team(name = "A4"), Team(name = "B5"))),
      (24, (Team(name = "B4"), Team(name = "A5")))
    ),
    List(
      (25, (Team(name = "W21"), Team(name = "B1"))),
      (26, (Team(name = "W22"), Team(name = "A1"))),
      (27, (Team(name = "L23"), Team(name = "L24 (*9)"))),
      (28, (Team(name = "W23"), Team(name = "W24 (*7)")))
    ),
    List(
      (29, (Team(name = "L25"), Team(name = "L26 ( 3*)"))),
      (30, (Team(name = "W25"), Team(name = "W26 ( 1*)")))
    )
  ).reverse

  val poolsRounds12: List[Round] = List(
    List(
      (1, (Team(id = 1), Team(id = 11))),
      (2, (Team(id = 9), Team(id = 7))),
      (3, (Team(id = 5), Team(id = 3))),

      (4, (Team(id = 2), Team(id = 12))),
      (5, (Team(id = 10), Team(id = 8))),
      (6, (Team(id = 6), Team(id = 4)))
    ),
    List(
      (7, (Team(id = 1), Team(id = 7))),
      (8, (Team(id = 11), Team(id = 3))),
      (9, (Team(id = 9), Team(id = 5))),

      (10, (Team(id = 2), Team(id = 8))),
      (11, (Team(id = 12), Team(id = 4))),
      (12, (Team(id = 10), Team(id = 6)))
    ),
    List(
      (13, (Team(id = 1), Team(id = 3))),
      (14, (Team(id = 5), Team(id = 7))),
      (15, (Team(id = 9), Team(id = 11))),

      (16, (Team(id = 2), Team(id = 4))),
      (17, (Team(id = 6), Team(id = 8))),
      (18, (Team(id = 10), Team(id = 12)))
    ),
    List(
      (19, (Team(id = 1), Team(id = 5))),
      (20, (Team(id = 9), Team(id = 3))),
      (21, (Team(id = 11), Team(id = 7))),

      (22, (Team(id = 2), Team(id = 6))),
      (23, (Team(id = 10), Team(id = 4))),
      (24, (Team(id = 12), Team(id = 8)))
    ),
    List(
      (25, (Team(id = 1), Team(id = 9))),
      (26, (Team(id = 11), Team(id = 5))),
      (27, (Team(id = 3), Team(id = 7))),

      (28, (Team(id = 2), Team(id = 10))),
      (29, (Team(id = 12), Team(id = 6))),
      (30, (Team(id = 4), Team(id = 8)))
    ),
    List(
      (31, (Team(name = "A1"), Team(name = "B4"))),
      (32, (Team(name = "A4"), Team(name = "B1"))),
      (33, (Team(name = "A2"), Team(name = "B3"))),
      (34, (Team(name = "A3"), Team(name = "B2"))),

      (35, (Team(name = "A5"), Team(name = "B6"))),
      (36, (Team(name = "A6"), Team(name = "B5")))
    ),
    List(
      (37, (Team(name = "W31"), Team(name = "W34"))),
      (38, (Team(name = "W32"), Team(name = "W33"))),
      (39, (Team(name = "L31"), Team(name = "L34"))),
      (40, (Team(name = "L32"), Team(name = "L33"))),

      (41, (Team(name = "L35"), Team(name = "L36 (11*)"))),
      (42, (Team(name = "W35"), Team(name = "W36 ( 9*)")))
    ),
    List(
      (43, (Team(name = "L39"), Team(name = "L40 ( 7*)"))),
      (44, (Team(name = "W39"), Team(name = "W40 ( 5*)"))),
      (45, (Team(name = "L37"), Team(name = "L38 ( 3*)"))),
      (46, (Team(name = "W37"), Team(name = "W38 ( 1*)")))
    )
  ).reverse

}

trait RoundFormatter {
  def fillRoundWithTeamNames(teamNames: Vector[String], round: Round): Round = {
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
