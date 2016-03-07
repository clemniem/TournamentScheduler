package Master

import Master.GameMode.GameMode
import Master.TeamState.TeamState
import Master.Types.Strength


/**
  * needed Datatypes
  */
case class Teem(val losses:Int, val lastGame:Int, val nextGame:Int, val state:TeamState)
case class Team(val id: Int = -1, val MeanStrength: Strength = -1, name: String = "")
case class TournamentMode(val startTime: String = "",
                          val endTime: String = "",
                          val gameTime: Int = 30,
                          val pauseTime: Int = 5,
                          val fields: Int = -1,
                          val days: Int = 1,
                          val gameMode: GameMode = GameMode.RoundRobin)
//todo add days and number of pools

object Types {
  type Strength = Int
  type Game = (Int,(Team,Team))
  type Round = List[Game]
  type Slot = String
}

object TeamState extends Enumeration {
  type TeamState = Value
  val Team, Winner, Looser, Place = Value
}

object GameMode extends Enumeration {
  type GameMode = Value
  val RoundRobin, Elimination, Pools = Value
}