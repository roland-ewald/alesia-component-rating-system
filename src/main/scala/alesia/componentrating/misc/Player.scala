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
package alesia.componentrating.misc

import scala.collection.immutable.List


/**
 * Represents a player within the TrueSkill RankCalculator. Note that this is not used outside the RankCalculator ({@see TrueSkillFactory}, {@see Factor_Prior}, {@see TrueSkillControl}).
 * The Set of Players is the Knowledgebase.
 *
 * @author Jonathan Wienss
 * @param skill Value representing the skill of this player. Initialise with standard skill ({@see dflt.defaultSkill}). Manipulate only from TrueSkillControl to keep an overview.
 * @param name the Name. Optional. Used for debugging.
 * @param id reference to an instance of ID for identification. Connects Player to the "real" player outside of rankcalculator, which it is representing. Optional, only needed if RankCalculator is used in a TrueSkillExperiment (if you use TrueSkillControl).
 *
 */
class Player(val skill: NormalDist, val id: String) {
    override def toString = id + ": " + skill
}
 
object Player {

  /**
   * Creates new player via (mu, sigma) of its skill.
   * @param mu the mean
   * @param sigma the standard deviation
   * @return new player with given skill
   */
  def apply(mu: Double, sigma: Double) =
    new Player(new NormalDist(1 / (sigma * sigma), 1 / (sigma * sigma) * mu), "") 

  def apply(mu: Double, sigma: Double, id: String) =
    new Player(new NormalDist(1 / (sigma * sigma), 1 / (sigma * sigma) * mu), id)
}

/**
 * A team contains a list of players. 
 */
class Team(val players: List[Player])

object Team {
  def apply(players: Player*) = new Team(List(players: _*))
}
