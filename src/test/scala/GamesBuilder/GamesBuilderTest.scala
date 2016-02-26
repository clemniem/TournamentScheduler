package GamesBuilder

import GamesBuilder.{Matches, TeamsToMatches}
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
    "get a Seq of Teams and return matches" in {
      val gameBuilder = system.actorOf(GamesBuilder.props, GamesBuilder.name)
      val testTeams = List(Team(1,10),Team(2,9),Team(3,8),Team(4,12))
      gameBuilder ! TeamsToMatches(testTeams,new TournamentMode())
      expectMsgPF(20.seconds) {
        case Matches(teams) =>
          println(teams)
          teams.size must be(6)
      }
    }
  }
}