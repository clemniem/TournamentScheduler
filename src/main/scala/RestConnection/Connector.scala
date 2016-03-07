package RestConnection


import GamesBuilder.GamesBuilder.TeamsToRounds
import Master.{Team, GameMode, TournamentMode, UeberActor}
import Master.UeberActor.FinishedSchedule
import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.{ConfigFactory, Config}
import akka.pattern.ask
import akka.http.scaladsl.Http
import spray.json.{DefaultJsonProtocol, _}
import akka.http.scaladsl.server.Directives._



import scala.concurrent.duration._

case class TournamentRequest(teams: List[String], startTime: String, endTime: String, gameTime: Int, pauseTime: Int, fields: Int, gameMode: Int, days: Int)

trait Protocols extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val tournamentRequest = jsonFormat8(TournamentRequest.apply)
  implicit val finishedScheduleResponse = jsonFormat1(FinishedSchedule.apply)
}

trait Service extends Protocols{
  implicit val system: ActorSystem
  implicit val materializer: ActorMaterializer
  import scala.concurrent.ExecutionContext.Implicits.global

  def config: Config
  val logger: LoggingAdapter

  val getTournamentRoute = logRequestResult("gameBuilder") {
    {
      path("getTournament") {
        (post & entity(as[TournamentRequest])) { tR =>
          implicit val timeout = Timeout(15.seconds)
          val ueberActor = system.actorOf(UeberActor.props)
          val tournamentMode = TournamentMode(tR.startTime, tR.endTime, tR.gameTime, tR.pauseTime, tR.fields, tR.days, GameMode(tR.gameMode))
          val teamList = toTeamList(tR.teams)
          println(s"Teamlist after Regex: $teamList")
          val futureAnswer = ueberActor ? TeamsToRounds(teamList, tournamentMode)
          import akka.http.scaladsl.model.StatusCodes._
          //todo try onSuccess
          complete {
            futureAnswer.map[ToResponseMarshallable]{
              case f @ FinishedSchedule(slots) => OK -> f.toJson
              case any => BadRequest -> "sorry"
            }
          }
        }
      }
    }
  }
  def toTeamList(teams: List[String]): List[Team] = {
    val pattern = """\((.*)\)""".r
    //todo extract only team name
    teams.filter(_.contains("Team")).zipWithIndex.map(tuple =>
      Team(id = tuple._2+1, meanStrength = pattern.findAllIn(tuple._1).matchData.next.group(1).toInt, name = tuple._1))
  }
}


object AkkaHttpMicroservice extends App with Service {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
  override val config = ConfigFactory.load()
  override val logger = Logging(system, getClass)
  Http().bindAndHandle(getTournamentRoute, "0.0.0.0", 4444)
}