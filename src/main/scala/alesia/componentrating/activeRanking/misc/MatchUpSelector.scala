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

import scala.collection.mutable.HashMap
import scala.util.Random
import alesia.componentrating.misc.Helper

/**
 * MatchUpSelector chooses the best team match-up according to provided MatchQuality.
 *
 * @see MatchQuality
 *
 * @author Jonathan Wienss
 *
 * @param <T> Type of the Teams (which can be List[String] or otherwise)
 */
abstract class MatchUpSelector[T](val matchQuality: MatchQuality[T]) {

  var allPossibleTeamUps: List[List[T]] = null // Error if not initialised!
  val allPossibleTeamUpsBase = HashMap[String, List[List[T]]]()
  var rng: Random = null

  def init(allTeams: List[T], teamsPerGame: Int, rng1: Random): Unit = {
    allPossibleTeamUps = allPossibleTeamUpsBase.getOrElseUpdate(allTeams.toString, Helper.linearCombination(allTeams, teamsPerGame, false))
    rng = rng1
  }
  def getMatchUp(): List[T]
}

class RandomMatchUpSelector[T](matchQuality: MatchQuality[T]) extends MatchUpSelector[T](matchQuality) {
  override def getMatchUp(): List[T] = {
    return Helper.chooseRand(allPossibleTeamUps, rng)
  }
}

class BestMatchUpSelector[T](matchQuality: MatchQuality[T]) extends MatchUpSelector[T](matchQuality) {
  override def getMatchUp(): List[T] = {
    val qualtities = allPossibleTeamUps.map(tu => (tu, matchQuality.getQuality(tu))).toMap
    return allPossibleTeamUps.sortBy(-1 * qualtities(_)).head
  }
}

class WeightedRandomMatchUpSelector[T](matchQuality: MatchQuality[T]) extends MatchUpSelector[T](matchQuality) {
  override def getMatchUp(): List[T] = {
    val qualtities = allPossibleTeamUps.map(tu => (tu, matchQuality.getQuality(tu))).toMap
    return Helper.chooseRandWeighted(allPossibleTeamUps, qualtities, rng)
  }
}
