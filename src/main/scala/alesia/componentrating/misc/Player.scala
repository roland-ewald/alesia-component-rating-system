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