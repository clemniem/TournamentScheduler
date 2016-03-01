package GamesBuilder

import GamesBuilder.{GameRounds, TeamsToMatches}
import Master.Types.{Game, Round}
import Master.{GameMode, TournamentMode, Team}
import akka.actor.{Actor, Props}

import scala.collection.immutable.::


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
    val games = flippedLowerHalf.zip(sortedTeams)
    Stream.from(1).zip((games.zipWithIndex.filter(_._2 % 2 == 0)
      ++ games.zipWithIndex.filter(_._2 % 2 != 0)).map(_._1)).toList :: Nil
  }


  def eliminationRecursive(rounds:List[Round]): List[Round] = rounds match {
    case seed :: Nil => seed :: eliminationRecursive(getNextRound(rounds))
    case wBracket :: lBracket :: Nil =>
      wBracket :: lBracket :: eliminationRecursive(getNextRound(wBracket::lBracket :: Nil :: Nil))
    case wBracket :: lBracket :: pBracket :: rs =>
      wBracket:: lBracket :: pBracket :: eliminationRecursive(wBracket :: lBracket :: pBracket :: Nil)
    case _ => Nil //ERROR
  }

  def getNextRound(rounds:List[Round]): List[Round] = rounds match {
    case seed :: Nil =>
      var wb:Round = Nil
      var lb:Round = Nil
      //getting list with two elements (= last node in tree)
      for( lastNode@(g1::g2::rs) <- seed.map(_._1).grouped(2)){
        wb ::= (g1+seed.size,(Team(-seed.size,0,s"W$g1"),Team(-seed.size,0,s"W$g2")))
        lb ::= (g2+seed.size,(Team(-seed.size,0,s"L$g1"),Team(-seed.size,0,s"L$g2")))
      }
      wb.reverse :: lb.reverse :: Nil

    case wBracket :: lBracket :: pBracket :: Nil =>
      wBracket match {
      case Nil => Nil
      case games =>
        val gameCount = -games.head._2._1.id
        var wb:Round = Nil
        var lb:Round = Nil
        var pb:Round = Nil
        //getting list with two elements (= last node in tree)
        for( lastNode@(g1::g2::rs) <- wBracket.map(_._1).grouped(2)) {
          wb ::= (g1+gameCount,   (Team(-gameCount, 0, s"W$g1"), Team(-gameCount, 0, s"W$g2")))
          lb ::= (g1+1+gameCount, (Team(-gameCount, 0, s"L$g1"), Team(-gameCount, 0, s"W${g1+1}")))
          lb ::= (g2+1+gameCount, (Team(-gameCount, 0, s"L$g2"), Team(-gameCount, 0, s"W${g2+1}")))
        }
        for (lastNode@(g1::g2::rs) <- lBracket.map(_._1).grouped(2)) {
          pb ::= (g1+1+gameCount,(Team(-gameCount,0,s"L$g1"),Team(-gameCount,0,s"L$g2")))
        }
        wb.reverse :: lb.reverse :: pb.reverse :: Nil
      }


  }

}





















//todo implement Pools-Mode-trait