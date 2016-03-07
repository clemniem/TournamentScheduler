package Scheduler

import GamesBuilder.{Elimination, GamesBuilder, StopSystemAfterAll}
import Master.Types.Round
import Master.UeberActor.{FinishedSchedule, MakeSchedule}
import Master.{GameMode, TournamentMode, Team}
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}

/**
  * Tests the GameBuilder
  */
class SchedulerRoundRobinTest extends TestKit(ActorSystem("testSys"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
  with StopSystemAfterAll
  with Elimination {

  "An Scheduler" must {
    val scheduler = system.actorOf(Scheduler.props, Scheduler.name)
    "get a List of Rounds and return List of Slots EVEN" in {
      val testRounds = List(
        List((1,(Team(2,9,""), Team(3,8,""))), (2,(Team(1,10,""), Team(4,12,"")))),
        List((3,(Team(4,12,""),Team(2,9,""))), (4,(Team(1,10,""), Team(3,8,"")))),
        List((5,(Team(3,8,""), Team(4,12,""))),(6,(Team(1,10,""), Team(2,9,"")))))
      val mode = new TournamentMode("10:00","20:00",30,5,2,1,GameMode.RoundRobin)
      scheduler ! MakeSchedule(testRounds,mode)
      expectMsgPF(){
        case FinishedSchedule(slots) =>
          println(s"EVEN: $slots")
          println(slots(2))
          slots(2).split(",").size must be(7)
      }
    }
    "get a List of Rounds and return List of Slots ODD" in {
      val testRounds = List(
        List((1,(Team(3,8,""),Team(4,12,""))), (2,(Team(2,9,""),Team(5,10,""))), (3,(Team(1,10,""),Team(6,12,"")))),
        List((4,(Team(2,9,""),Team(3,8,""))), (5,(Team(1,10,""),Team(4,12,""))), (6,(Team(7,10,""),Team(5,10,"")))),
        List((7,(Team(1,10,""),Team(2,9,""))), (8,(Team(7,10,""),Team(3,8,""))), (9,(Team(6,12,""),Team(4,12,"")))),
        List((10,(Team(7,10,""),Team(1,10,""))), (11,(Team(6,12,""),Team(2,9,""))), (12,(Team(5,10,""),Team(3,8,"")))),
        List((13,(Team(6,12,""),Team(7,10,""))), (14,(Team(5,10,""),Team(1,10,""))), (15,(Team(4,12,""),Team(2,9,"")))),
        List((16,(Team(5,10,""),Team(6,12,""))), (17,(Team(4,12,""),Team(7,10,""))), (18,(Team(3,8,""),Team(1,10,"")))),
        List((19,(Team(4,12,""),Team(5,10,""))), (20,(Team(3,8,""),Team(6,12,""))), (21,(Team(2,9,""),Team(7,10,"")))))

      val mode = new TournamentMode("10:00","20:00",30,5,2,1,GameMode.RoundRobin)
      scheduler ! MakeSchedule(testRounds,mode)
      expectMsgPF(){
        case FinishedSchedule(slots) =>
          println(s"ODD : $slots")
          slots(2).split(",").size must be(7)
      }
    }

    "get a List of Rounds for Elimination and provide Schedule" in {
      val elimListTemplate:List[Round] = List(
        List(
          (1, (Team(id = 1), Team(id = 8))),
          (2, (Team(id = 3), Team(id = 6))),
          (3, (Team(id = 2), Team(id = 7))),
          (4, (Team(id = 4), Team(id = 5)))
        ),
        List(
          (5, (Team(name = "L1"), Team(name = "L2"))),
          (6, (Team(name = "L2"), Team(name = "L3"))),
          (7, (Team(name = "L1"), Team(name = "L2"))),
          (8, (Team(name = "W3"), Team(name = "W4")))
        ),
        List(
          (9, (Team(name = "L5"), Team(name = "L6"))),
          (10, (Team(name = "W5"), Team(name = "W6"))),
          (11, (Team(name = "L7"), Team(name = "L8"))),
          (12, (Team(name = "W7"), Team(name = "W8")))
        )
      )
      val elimList = elimListTemplate.map(r => fillRoundsWithTeamNames(Vector(
        "Dummy", "Strong","Second","Third","Fourth","Fifth","Sixth","seventh","eighth"),r))
      val mode = new TournamentMode("10:00","20:00",30,5,3,1,GameMode.Elimination)
      scheduler ! MakeSchedule(elimList,mode)
      expectMsgPF(){
        case FinishedSchedule(slots) =>
          println(s"Elim : $slots")
          //slots(2).split(",").size must be(7)
      }
    }

    "check single Functions for working" in {
      val underScheduler: TestActorRef[GamesBuilder] = TestActorRef(GamesBuilder.props)
      val us = underScheduler.underlyingActor
      val testRounds = List(
        List((1,(Team(3,8,""),Team(4,12,""))), (2,(Team(2,9,""),Team(5,10,""))), (3,(Team(1,10,""),Team(6,12,"")))),
        List((4,(Team(2,9,""),Team(3,8,""))), (5,(Team(1,10,""),Team(4,12,""))), (6,(Team(7,10,""),Team(5,10,"")))),
        List((7,(Team(1,10,""),Team(2,9,""))), (8,(Team(7,10,""),Team(3,8,""))), (9,(Team(6,12,""),Team(4,12,"")))),
        List((10,(Team(7,10,""),Team(1,10,""))), (11,(Team(6,12,""),Team(2,9,""))), (12,(Team(5,10,""),Team(3,8,"")))),
        List((13,(Team(6,12,""),Team(7,10,""))), (14,(Team(5,10,""),Team(1,10,""))), (15,(Team(4,12,""),Team(2,9,"")))),
        List((16,(Team(5,10,""),Team(6,12,""))), (17,(Team(4,12,""),Team(7,10,""))), (18,(Team(3,8,""),Team(1,10,"")))),
        List((19,(Team(4,12,""),Team(5,10,""))), (20,(Team(3,8,""),Team(6,12,""))), (21,(Team(2,9,""),Team(7,10,"")))))

      val elimGrouping = testRounds.flatMap(_.grouped(2))
      println(elimGrouping)
    }
  }
}
