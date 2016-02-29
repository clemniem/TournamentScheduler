package Scheduler

import Master.{GameMode, TournamentMode}
import Master.Types.Rounds
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

class Scheduler extends Actor {

  def receive: Receive = {
    case MakeSchedule(rounds,mode) => mode.gameMode match {
      case GameMode.RoundRobin => sender ! FinishedSchedule (roundsToScheduleRR (mode, rounds))
      //todo implement other Cases
      case _ => sender ! FinishedSchedule (roundsToScheduleRR (mode, rounds))
    }
  }

  def roundsToScheduleRR(mode:TournamentMode, rounds: Rounds): List[String] = {
    var acc = ""
    var result: List[String] = Nil
    val format = DateTimeFormat.forPattern("hh:mm")
    var time = DateTime.parse(mode.startTime,format)
    for (slots <- rounds.flatten.grouped(mode.fields)){
        acc = s"${format.print(time)}-${format.print(time + mode.gameTime.minutes)},"
        for(game@(t1,t2) <- slots){
          acc += s",Team ${t1.id},Team ${t2.id},"
        }
        time += mode.gameTime.minutes + mode.pauseTime.minutes
        result = acc :: result
      }
      result.reverse
  }

}