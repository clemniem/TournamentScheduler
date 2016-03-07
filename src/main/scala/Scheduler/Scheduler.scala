package Scheduler

import Master.{Team, GameMode, TournamentMode}
import Master.Types.{Game, Slot, Round}
import Master.UeberActor.{FinishedSchedule, MakeSchedule}
import akka.actor.{Actor, Props}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import com.github.nscala_time.time.Imports._


/**
  * This actor gets a List of Rounds and returns a List of Slots
  */
object Scheduler {
  val name = "scheduler"
  val props = Props(new Scheduler)

}

class Scheduler extends Actor with Log2 with FormatHelpers{

  def receive: Receive = {
    case MakeSchedule(rounds, mode) => mode.gameMode match {
      case GameMode.RoundRobin => sender ! FinishedSchedule(roundsToSchedule(mode, rounds))
      case GameMode.Elimination => sender ! FinishedSchedule(roundsToSchedule(mode, rounds))

      //todo implement other Cases
      case _ => sender ! FinishedSchedule(roundsToSchedule(mode, rounds))
    }
  }

  def roundsToSchedule(mode: TournamentMode, rounds: List[Round]): List[String] = {
    var results: List[String] = initStringForTournament(mode) :: Nil
    var acc = ""

    val format = DateTimeFormat.forPattern("hh:mm")
    var time = DateTime.parse(mode.startTime, format)
    // Groups them in to the number of available fields (with/without)
    // formats each slot for .csv
    //todo check if team is playing twice in one slot: ERROR
    for (slots <- mode.gameMode match {
      case GameMode.RoundRobin =>  rounds.flatten.grouped(mode.fields)    //without empty fields per slot
      case _ => rounds.flatMap(_.grouped(mode.fields)) //with empty fields per slot
    }) {
      acc = timeStringForSlot(time, mode)
      for (game@(gameId, (t1, t2)) <- slots) {
        acc += gameToString(game)
      }
      for(empty <- 0 until mode.fields-slots.size){
        acc += ",,--- : ---,"
      }
      time += mode.gameTime.minutes + mode.pauseTime.minutes
      results = acc :: results
    }
    results.reverse
  }
}


trait FormatHelpers {

  def gameToString(game: Game): String = game match {
    case g@(gameId, (t1, t2)) =>
      s",$gameId,${
        t1.name match {
          case "" => s"Team ${t1.id}"
          case name => name
        }
      }: ${
        t2.name match {
          case "" => s"Team ${t2.id}"
          case name => name
        }
      },"
  }

  def initStringForTournament(mode: TournamentMode):String = {
    var acc = "Time,"
    for (nr <- 1 to mode.fields) {
      acc += s",,Field $nr ,"
    }
    acc
  }

  def timeStringForSlot(time:DateTime,mode:TournamentMode) = {
    val format = DateTimeFormat.forPattern("hh:mm")

    s"${format.print(time)}-${format.print(time + mode.gameTime.minutes)},"
  }

}


trait Log2 {
  val lnOf2 = scala.math.log(2)
  // natural log of 2
  def log2(x: Int): Int = (scala.math.log(x.toDouble) / lnOf2).toInt
}


