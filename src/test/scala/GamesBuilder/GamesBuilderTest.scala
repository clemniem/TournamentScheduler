package GamesBuilder

import GamesBuilder.{GameRounds, TeamsToMatches}
import Master.{TournamentMode, Team}
import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
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
      val testTeamsEven = List(Team(1,10),Team(2,9),Team(3,8),Team(4,12))
      gameBuilder ! TeamsToMatches(testTeamsEven,new TournamentMode())
      expectMsgPF(20.seconds) {
        case GameRounds(teams) =>
          println(teams)
          teams.flatten.size must be(6) // == number of total matches
      }
    }
    "get a Seq of odd Teams and return matches" in {
      val testTeamsOdd = List(Team(1,10),Team(2,9),Team(3,8),Team(4,12),Team(5,10),Team(6,12),Team(7,10))
      gameBuilder ! TeamsToMatches(testTeamsOdd,new TournamentMode())
      expectMsgPF(20.seconds) {
        case GameRounds(teams) =>
          println(teams)
          teams.flatten.size must be(21) // == number of total matches
      }
    }
  }
}