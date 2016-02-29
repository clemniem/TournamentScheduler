package Scheduler

import GamesBuilder.StopSystemAfterAll
import Master.UeberActor.{FinishedSchedule, MakeSchedule}
import Master.{GameMode, TournamentMode, Team}
import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}

/**
  * Tests the GameBuilder
  */
class SchedulerRoundRobinTest extends TestKit(ActorSystem("testSys"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
  with StopSystemAfterAll {

  "An Scheduler" must {
    val scheduler = system.actorOf(Scheduler.props, Scheduler.name)
    "get a List of Rounds and return List of Slots EVEN" in {
      val testRounds = List(
        List((Team(2,9,""), Team(3,8,"")), (Team(1,10,""), Team(4,12,""))),
        List((Team(4,12,""),Team(2,9,"")), (Team(1,10,""), Team(3,8,""))),
        List((Team(3,8,""), Team(4,12,"")),(Team(1,10,""), Team(2,9,""))))
      val mode = new TournamentMode("10:00","20:00",30,5,2,1,GameMode.RoundRobin)
      scheduler ! MakeSchedule(testRounds,mode)
      expectMsgPF(){
        case FinishedSchedule(slots) =>
          println(s"EVEN: $slots")
          slots.head.split(",").size must be(7)
      }
    }
    "get a List of Rounds and return List of Slots ODD" in {
      val testRounds = List(
        List((Team(3,8,""),Team(4,12,"")), (Team(2,9,""),Team(5,10,"")), (Team(1,10,""),Team(6,12,""))),
        List((Team(2,9,""),Team(3,8,"")), (Team(1,10,""),Team(4,12,"")), (Team(7,10,""),Team(5,10,""))),
        List((Team(1,10,""),Team(2,9,"")), (Team(7,10,""),Team(3,8,"")), (Team(6,12,""),Team(4,12,""))),
        List((Team(7,10,""),Team(1,10,"")), (Team(6,12,""),Team(2,9,"")), (Team(5,10,""),Team(3,8,""))),
        List((Team(6,12,""),Team(7,10,"")), (Team(5,10,""),Team(1,10,"")), (Team(4,12,""),Team(2,9,""))),
        List((Team(5,10,""),Team(6,12,"")), (Team(4,12,""),Team(7,10,"")), (Team(3,8,""),Team(1,10,""))),
        List((Team(4,12,""),Team(5,10,"")), (Team(3,8,""),Team(6,12,"")), (Team(2,9,""),Team(7,10,""))))

      val mode = new TournamentMode("10:00","20:00",30,5,2,1,GameMode.RoundRobin)
      scheduler ! MakeSchedule(testRounds,mode)
      expectMsgPF(){
        case FinishedSchedule(slots) =>
          println(s"ODD : $slots")
          slots.head.split(",").size must be(7)
      }
    }
  }
}
