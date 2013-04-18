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
package alesia.componentrating.factorgraph

import org.junit.Assert.assertEquals
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import alesia.componentrating.misc.Player
import alesia.componentrating.misc.Team
import scala.collection.immutable.List
import alesia.componentrating.misc.TrueSkillDefaultValues
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.weiglewilczek.slf4s.Logging

/**
 * Compares results with the TrueSkill Online Rank Calculator by Microsoft.
 *
 * @see http://atom.research.microsoft.com/trueskill/rankcalculator.aspx
 *
 * @author Jonathan Wienss
 * @author Michael Stein
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class TestTrueSkillVSOnlineRankCalc extends FunSpec with Logging {

  val dflt = new TrueSkillDefaultValues(delta = 1e-08, debug = false)

  /** Comparison accuracy for numerical values; a larger difference leads to a failed assertion. */
  val delta = 1e-03

  describe("The TrueSkill implementation") {

    it("computes the same result for a simple two-player example") {
      checkSinglePlayerTeams(
        List(
          Player(10.0, 13.0, "A"),
          Player(35.0, 14.0, "B")),
        List(
          Player(24.877, 10.469, "A"),
          Player(17.747, 10.775, "B")))
    }

    it("computes the same result for three single-player teams") {
      checkSinglePlayerTeams(
        List(
          Player(10.0, 13.0, "A"),
          Player(35.0, 14.0, "B"),
          Player(5.0, 5.0, "C")),
        List(
          Player(26.885, 9.549, "A"),
          Player(20.831, 9.140, "B"),
          Player(4.309, 4.833, "C")))
    }

    it("computes the same result for a multi-player three-team game") {

      val teams = List(
        Team(
          Player(10.0, 13.0, "A")),
        Team(
          Player(35.0, 14.0, "B"),
          Player(5.0, 5.0, "C"),
          Player(15.0, 6.0, "D")),
        Team(
          Player(20.0, 7.0, "E"),
          Player(40.0, 8.0, "F")))

      val playersAfterUpdate = List(
        Player(44.555, 8.908, "A"),
        Player(27.057, 10.047, "B"),
        Player(3.987, 4.844, "C"),
        Player(13.541, 5.727, "D"),
        Player(11.966, 6.403, "E"),
        Player(29.507, 7.096, "F"))

      checkTeams(teams, playersAfterUpdate)
    }
  }

  /**
   * Compares some players with their expected skill after analyzing a game result.
   * The result is defined by the order of the players in the list, which has to be the same for the list of expected player skills.
   * Each player will be put into its own team.
   * @param playersBeforeUpdate the list of players before the update
   * @param playersAfterUpdate the list of expected player skill profiles after the update
   */
  def checkSinglePlayerTeams(playersBeforeUpdate: List[Player], playersAfterUpdate: List[Player]) =
    checkTeams(playersBeforeUpdate.map(Team(_)), playersAfterUpdate)

  /**
   * Retrieves result for a game among some teams and compares them with the expected updated skills.
   * The order of players in the teams has to match the order of players in the expected skill list; the team order also represents the game result.
   * @param teams the list of teams (result of the game)
   * @param playersAfterUpdate the list of expected updates
   */
  def checkTeams(teams: List[Team], playersAfterUpdate: List[Player]) = {
    val updatedPlayers = UpdateCalculator.factorGraphResults(teams)(dflt)
    updatedPlayers.map(x => x).foreach(x => logger.info("Player: " + x))
    playersAfterUpdate.foreach(p1 => { assertPlayerResultsExist(p1, updatedPlayers); updatedPlayers.foreach(p2 => if (p1.id == p2.id) check(p1, p2)) })
  }

  /**
   * Compares the skill of two players.
   * @param players the player tuple (first player is the expected result)
   */
  def check(players: (Player, Player)) {
    logger.debug("Player comparison (expected, actual):" + players)
    assertEquals("Mean should be equal", players._1.skill.mean, players._2.skill.mean, delta)
    assertEquals("StdDev should be equal", players._1.skill.stDev, players._2.skill.stDev, delta)
  }

  /**
   * Assures that a players id is the id of at least one player in a list of players
   * @param p the players id is what is of concern
   * @param l the list of players which should contain the id
   */
  def assertPlayerResultsExist(p: Player, l: List[Player]) {
    if (l.size == 1) assertEquals("Player does not show up in results", p.id, l.head.id)
    else if (p.id == l.head.id) assert(true)
    else assertPlayerResultsExist(p, l.tail)
  }

}
