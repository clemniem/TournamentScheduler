package GamesBuilder

import GamesBuilder.{GameRounds, TeamsToMatches}
import Master.{GameMode, TournamentMode, Team}
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}
import scala.concurrent.duration._


/**
  * Tests the GameBuilder
  */
class GamesBuilderTest extends TestKit(ActorSystem("testSys"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
  with StopSystemAfterAll {

  "An GameBuilder" must {
    val gameBuilder = system.actorOf(GamesBuilder.props, GamesBuilder.name)
    "get a Seq of even Teams and return matches" in {
      val testTeamsEven = List(Team(1, 10, ""), Team(2, 9, ""), Team(3, 8, ""), Team(4, 12, ""))
      gameBuilder ! TeamsToMatches(testTeamsEven, new TournamentMode())
      expectMsgPF(20.seconds) {
        case GameRounds(teams) =>
          println(teams)
          teams.flatten.size must be(6) // == number of total matches
      }
    }
    "get a Seq of odd Teams and return matches" in {
      val testTeamsOdd = List(Team(1, 10, ""), Team(2, 9, ""), Team(3, 8, ""), Team(4, 12, ""), Team(5, 10, ""), Team(6, 12, ""), Team(7, 10, ""))
      gameBuilder ! TeamsToMatches(testTeamsOdd, new TournamentMode())
      expectMsgPF(20.seconds) {
        case GameRounds(teams) =>
          println(teams)
          teams.flatten.size must be(21) // == number of total matches
      }
    }
//    "get a Seq of even Teams and return matches for Elimination" in {
//      val testTeamsOdd = List(Team(1, 10, ""), Team(2, 9, ""), Team(3, 8, ""), Team(4, 12, ""), Team(5, 10, ""), Team(6, 12, ""))
//      gameBuilder ! TeamsToMatches(testTeamsOdd, new TournamentMode("", "", 0, 0, 2, 2, GameMode.Elimination))
//      expectMsgPF(20.seconds) {
//        case GameRounds(teams) =>
//          println(teams)
//          teams.flatten.size must be(3) // == number of total matches
//      }
//    }

    "calculate the team mean vector when inputting the team vectors" in {
      val underGameBuilder: TestActorRef[GamesBuilder] = TestActorRef(GamesBuilder.props)
      val testTeams16 = List(
        Team(1, 8, ""), Team(2, 4, ""), Team(3, 3, ""), Team(4, 7, ""),
        Team(5, 1, ""), Team(6, 5, ""), Team(3, 2, ""), Team(8, 6, ""),
        Team(9, 9, ""), Team(10, 10, ""), Team(11, 11, ""), Team(12, 12, ""),
        Team(13, 13, ""), Team(14, 14, ""), Team(15, 15, ""), Team(16, 15, ""))

      val seed = underGameBuilder.underlyingActor.seedForElimination(testTeams16)
      println(seed)
      val firstRound = underGameBuilder.underlyingActor.getNextRound(List(List((1,(Team(5,1,""),Team(1,8,""))), (2,(Team(3,2,""),Team(4,7,""))), (3,(Team(3,3),Team(8,6))), (4,(Team(2,4),Team(6,5))))))
      println(firstRound)
      val secondRound = underGameBuilder.underlyingActor.getNextRound(List(
        List((5,(Team(-4,0,"W1"),Team(-4,0,"W2"))), (7,(Team(-4,0,"W3"),Team(-4,0,"W4")))),
        List((6,(Team(-4,0,"L1"),Team(-4,0,"L2"))), (8,(Team(-4,0,"L3"),Team(-4,0,"L4"))))))
      println(secondRound)
      val testSchedule = underGameBuilder.underlyingActor.roundsForElimination(testTeams16)
      println(testSchedule)
    }
  }

}