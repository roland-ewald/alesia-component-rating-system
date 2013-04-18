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
package alesia.componentrating

import org.junit.runner.RunWith
import org.junit.Assert.assertEquals
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import alesia.componentrating.misc.Helper
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.weiglewilczek.slf4s.Logging

/**
 * Tests <code>TrueSkillRankingSystem</code>.
 *
 * @author Jonathan Wienss
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class TestTrueSkillRatingSystem extends FunSpec with Logging {

  /** Players have an identifier, a strength, and a true ranking. */
  type PlayerProperties = (String, Int, Int)

  /** Test players. */
  val players = List(
    ("Component A", 5, 3),
    ("Component B", 3, 4),
    ("Component C", 7, 2),
    ("Component D", 10, 1))

  describe("Four Components are ranked, 6 combinations") {
    testSimpleRatingProblem(players, Helper.linearCombination(players, 2, false)) // 2 teams per game, no double occurrence of one team)
  }

  describe("Four Components are ranked, 5 combinations") {
    testSimpleRatingProblem(players, List(
      List(players(0), players(1)),
      List(players(2), players(3)),
      List(players(0), players(3)),
      List(players(1), players(2)),
      List(players(2), players(0))))
  }

  describe("Four Components are ranked in 2 teams of 2") {
    testMultiRatingProblem(players, Helper.linearCombination(Helper.linearCombination(players, 2, false), 2, false))
  }

  // Here: Each team consists of only one component (Player)
  private def testSimpleRatingProblem(ratingProblem: List[PlayerProperties], ganes: List[List[PlayerProperties]]) {

    val tsrs = new TrueSkillRatingSystem

    val combinations = ganes.map(l => l.sortBy(-_._2)).map(x => x.map(y => Set(y._1))) // Strength determines winning team
    combinations.foreach(tsrs.submitResults)

    val ranking = ratingProblem.sortBy(_._3).map(x => x._1)
    assertCorrectRanking(ranking, tsrs)
  }

  // Here: Each team consists of only multiple components (player)
  private def testMultiRatingProblem(ratingProblem: List[Tuple3[String, Int, Int]], games: List[List[List[Tuple3[String, Int, Int]]]]) {
    val tsrs = new TrueSkillRatingSystem()

    val combinations = games.map(l => {
      l.sortBy( // Here sorting is used to put winning team first ...
        l.map(l2 => { (l2, l2.map(_._2.toDouble).foldLeft(.0)(_ + _)) }).toMap // .. for that we need a map team -> team strength
        ).reverse.map(x => x.toSet) // Teams get rearranged from List to Set here
    }).map(_.map(_.map(_._1))) // prune Component representation down to be only a String
    combinations.foreach(tsrs.submitResults)

    val ranking = ratingProblem.sortBy(_._3).map(x => x._1)
    assertCorrectRanking(ranking, tsrs)
  }

  private def assertCorrectRanking(l: List[String], tsrs: TrueSkillRatingSystem) {
    if (l.size == 1) return
    l.tail.foreach(x => {
      it("asserts the correct Ranking of " + l.head + " and " + x) {
        assertEquals("Correct Ranking", tsrs.compare(l.head, x), 1)
        assertEquals("Correct Ranking", tsrs.compare(x, l.head), -1)
      }
    })
    assertCorrectRanking(l.tail, tsrs)
  }
}
