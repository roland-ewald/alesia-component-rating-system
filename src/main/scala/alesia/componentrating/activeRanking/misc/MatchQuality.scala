/*******************************************************************************
 * Copyright 2012-2013 Jonathan Wienss, Michael Stein, Roland Ewald
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package alesia.componentrating.activeRanking.misc

import alesia.componentrating.ComponentRatingSystem
import alesia.componentrating.misc.TrueSkillDefaultValues
import alesia.componentrating.misc.AdvancedOptions

/**
 * Quality of a match-up between available component teams.
 * Used to determine the "best" match-up: the higher the quality, the better.
 *
 * @author Jonathan Wienss
 */
abstract class MatchQuality[T]()(implicit dflt: TrueSkillDefaultValues, advOpt: AdvancedOptions) {
  var teams: List[T] = null // all possible Teams 
  var getTeamPlayers: Map[T, List[String]] = null // Teams -> List (Players) [NOTE: Teams can be any objects]
  var crs: ComponentRatingSystem = null

  var useAverage = advOpt.usePPBalancing || advOpt.useVPBalancing

  def init(allTeams: List[T], getTeamPlayers1: Map[T, List[String]], crs1: ComponentRatingSystem) = {
    teams = allTeams; getTeamPlayers = getTeamPlayers1; crs = crs1
    useAverage = advOpt.usePPBalancing
  }
  def getQuality(teams: List[T]): Double
}

/**
 * Prefer MatchUps where Teams have equal Points
 */
class SimilarPointsMatchQuality[T]()(implicit dflt: TrueSkillDefaultValues, advOpt: AdvancedOptions) extends MatchQuality[T]() {
  var playerPoints: List[List[Double]] = null
  var teamPointMap: Map[T, Double] = null
  override def init(allTeams: List[T], getTeamPlayers1: Map[T, List[String]], crs1: ComponentRatingSystem) = {
    super.init(allTeams, getTeamPlayers1, crs1)
    playerPoints = teams.map(getTeamPlayers).map(team => team.map(crs.getPoints))
    def teamPoints(list: List[Double]): Double = { var i = 0.0; list.foreach(t => i = i + t); if (useAverage) i = i / list.size; i }
    teamPointMap = (teams zip playerPoints.map(teamPoints)).toMap
  }
  override def getQuality(game: List[T]): Double = {
    var averageTeamUpPoints = 0.0
    game.map(teamPointMap).foreach(p => averageTeamUpPoints = averageTeamUpPoints + p)
    averageTeamUpPoints = averageTeamUpPoints / game.size

    var result = 0.0
    game.map(teamPointMap).map(p => scala.math.abs(averageTeamUpPoints - p)).foreach(p => result = result + p)
    result = result / game.size

    return 20.0 / (1.0 + result) // Up to MQ of 20
  }
}

/**
 *
 */
class AverageUncertaintyMatchQuality[T]()(implicit dflt: TrueSkillDefaultValues, advOpt: AdvancedOptions) extends MatchQuality[T]() {
  var playerUncertainty: List[List[Double]] = null
  var teamUncertaintyMap: Map[T, Double] = null
  override def init(allTeams: List[T], getTeamPlayers1: Map[T, List[String]], crs1: ComponentRatingSystem) = {
    super.init(allTeams, getTeamPlayers1, crs1)
    playerUncertainty = teams.map(getTeamPlayers).map(team => team.map(crs.getUncertainty))
    def teamPoints(list: List[Double]): Double = { var i = 0.0; list.foreach(t => i = i + t); if (useAverage) i = i / list.size; i }
    teamUncertaintyMap = (teams zip playerUncertainty.map(teamPoints)).toMap
  }
  override def getQuality(game: List[T]): Double = {
    var averageTeamUpUncertainty = 0.0
    game.map(teamUncertaintyMap).foreach(p => averageTeamUpUncertainty = averageTeamUpUncertainty + p)
    averageTeamUpUncertainty = averageTeamUpUncertainty / game.size

    return averageTeamUpUncertainty
  }
}

/**
 *
 */
class PointsAndUncertaintyMatchQuality[T]()(implicit dflt: TrueSkillDefaultValues, advOpt: AdvancedOptions) extends MatchQuality[T]() {

  var playerPoints: List[List[Double]] = null // Points of each Player
  var teamPointMap: Map[T, Double] = null // Team -> Points of this Teams
  var playerUncertainty: List[List[Double]] = null // uncertainty of Players
  var teamUncertaintyMap: Map[T, Double] = null // Team -> Uncertainty

  override def init(allTeams: List[T], getTeamPlayers1: Map[T, List[String]], crs1: ComponentRatingSystem) = {
    super.init(allTeams, getTeamPlayers1, crs1)
    playerPoints = teams.map(getTeamPlayers).map(team => team.map(crs.getPoints))
    def teamPoints(list: List[Double]): Double = { var i = 0.0; list.foreach(t => i = i + t); if (useAverage) i = i / list.size; i } // sums up points of the teams players
    teamPointMap = (teams zip playerPoints.map(teamPoints)).toMap
    playerUncertainty = teams.map(getTeamPlayers).map(team => team.map(crs.getUncertainty))
    def teamUc(list: List[Double]): Double = { var i = 0.0; list.foreach(t => i = i + t); if (useAverage) i = i / list.size; i }
    teamUncertaintyMap = (teams zip playerUncertainty.map(teamUc)).toMap
  }

  override def getQuality(game: List[T]): Double = {
    var averageTeamUpPoints = 0.0
    game.map(teamPointMap).foreach(p => averageTeamUpPoints = averageTeamUpPoints + p)
    averageTeamUpPoints = averageTeamUpPoints / game.size

    var result = 0.0
    game.map(teamPointMap).map(p => scala.math.abs(averageTeamUpPoints - p)).foreach(p => result = result + p)
    result = result / game.size

    var averageTeamUpUncertainty = 0.0
    game.map(teamUncertaintyMap).foreach(p => averageTeamUpUncertainty = averageTeamUpUncertainty + p)
    averageTeamUpUncertainty = averageTeamUpUncertainty / game.size

    return 20.0 / (1.0 + result) + averageTeamUpUncertainty // Up to MQ of 20
  }
}
