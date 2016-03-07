package GamesBuilder

import GamesBuilder.{GameRounds, TeamsToRounds}
import Master._
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
      gameBuilder ! TeamsToRounds(testTeamsEven, new TournamentMode())
      expectMsgPF(20.seconds) {
        case GameRounds(teams) =>
          //println(teams)
          teams.flatten.size must be(6) // == number of total matches
      }
    }
    "get a Seq of odd Teams and return matches" in {
      val testTeamsOdd = List(Team(1, 10, ""), Team(2, 9, ""), Team(3, 8, ""), Team(4, 12, ""), Team(5, 10, ""), Team(6, 12, ""), Team(7, 10, ""))
      gameBuilder ! TeamsToRounds(testTeamsOdd, new TournamentMode())
      expectMsgPF(20.seconds) {
        case GameRounds(teams) =>
          //println(teams)
          teams.flatten.size must be(21) // == number of total matches
      }
    }


    "calculate the team mean vector when inputting the team vectors" in {
      val underGameBuilder: TestActorRef[GamesBuilder] = TestActorRef(GamesBuilder.props)
      val testTeams16 = List(
        Team(1, 8, ""), Team(2, 4, ""), Team(3, 3, ""), Team(4, 7, ""),
        Team(5, 1, ""), Team(6, 5, ""), Team(3, 2, ""), Team(8, 6, ""),
        Team(9, 9, ""), Team(10, 10, ""), Team(11, 11, ""), Team(12, 12, ""),
        Team(13, 13, ""), Team(14, 14, ""), Team(15, 15, ""), Team(16, 15, "")
      )
      val testTeams8 = List(
        Team(1, 8, "Acht"), Team(2, 4, "Vier"), Team(3, 3, "Drei"), Team(4, 7, "Sieben"),
        Team(5, 1, "Eins"), Team(6, 5, "Funf"), Team(3, 2, "Zwei"), Team(8, 6, "sechs")
      )
      val testTeams4 = List(
        Team(1,8,"Stark"), Team(2, 4, "Schwach"), Team(3, 3, "Schwachest"), Team(4, 7, "Mittel"))
      val ua = underGameBuilder.underlyingActor
      val rounds4 = ua.roundsForElimination(testTeams4)
      println(rounds4)
      val rounds8 = ua.roundsForElimination(testTeams8)
      println(rounds8)




    }
  }

}