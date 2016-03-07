package Scheduler

import Master.{Team, GameMode, TournamentMode}
import Master.Types.Round
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

class Scheduler extends Actor with Log2 {

  def receive: Receive = {
    case MakeSchedule(rounds, mode) => mode.gameMode match {
      case GameMode.RoundRobin => sender ! FinishedSchedule(roundsToScheduleRR(mode, rounds))
      case GameMode.Elimination => sender ! FinishedSchedule(roundsToScheduleElimination(mode, rounds))

      //todo implement other Cases
      case _ => sender ! FinishedSchedule(roundsToScheduleRR(mode, rounds))
    }
  }

  def roundsToScheduleRR(mode: TournamentMode, rounds: List[Round]): List[String] = {
    var acc = ""
    var results: List[String] ={
      acc = "Time,"
      for (nr <- 1 to  mode.fields) {
      acc += s",Field $nr,"
    }
    acc :: Nil}
    val format = DateTimeFormat.forPattern("hh:mm")
    var time = DateTime.parse(mode.startTime, format)
    // Flattens all Rounds and Groups them in to the number of available fields
    // formats each slot for .csv
    // todo check if team is playing twice in one slot: ERROR
    for (slots <- rounds.flatten.grouped(mode.fields)) {
      acc = s"${format.print(time)}-${format.print(time + mode.gameTime.minutes)},"
      for (game@(gameId, (t1, t2)) <- slots) {
        acc += s",$gameId,${
          t1.name match {
            case "" => s"Team ${t1.id}"
            case name => name
          }
        },${
          t2.name match {
            case "" => s"Team ${t2.id}"
            case name => name
          }
        },"
      }
      time += mode.gameTime.minutes + mode.pauseTime.minutes
      results = acc :: results
    }
    results.reverse
  }

  def roundsToScheduleElimination(mode: TournamentMode, rounds: List[Round]): List[String] = {
    var noOfRounds = log2(rounds.head.size)
    var acc = ""
    var results: List[String] = Nil

    val format = DateTimeFormat.forPattern("hh:mm")
    var time = DateTime.parse(mode.startTime, format)
    //first round
    for (slots <- rounds.flatten.grouped(mode.fields)) {
      acc = s"${format.print(time)}-${format.print(time + mode.gameTime.minutes)},"
      for (game@(gameId, (t1, t2)) <- slots) {
        acc += s",$gameId,Team ${t1.id},Team ${t2.id},"
      }
      time += mode.gameTime.minutes + mode.pauseTime.minutes
      results = acc :: results
    }
    //future rounds
    for (r <- 1 to noOfRounds) {
      for (t <- 0 until rounds.head.size) {

      }
    }
    //todo
    results.reverse
  }


}

trait Log2 {
  val lnOf2 = scala.math.log(2)

  // natural log of 2
  def log2(x: Int): Int = (scala.math.log(x.toDouble) / lnOf2).toInt
}


