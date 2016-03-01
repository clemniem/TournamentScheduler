package Master

import Master.GameMode.GameMode
import Master.Types.Strength


/**
  * needed Datatypes
  */

case class Team(val id: Int, val MeanStrength: Strength, name: String = "")
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
}

object GameMode extends Enumeration {
  type GameMode = Value
  val RoundRobin, Elimination, Pools = Value
}