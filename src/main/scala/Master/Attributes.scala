package Master

import Master.GameMode.GameMode
import Master.Types.Strength


/**
  * needed Datatypes
  */

case class Team(val name: String, val MeanStrength: Strength)
case class TournamentMode(val startTime: Int = -1, val gameTime: Int = -1, val pauseTime: Int = -1,
                          val fields: Int = -1, val gameMode: GameMode = GameMode.RoundRobin)
//todo add days and number of pools

object Types {
  type Strength = Int
}

object GameMode extends Enumeration {
  type GameMode = Value
  val RoundRobin, Elimination, Pools = Value
}