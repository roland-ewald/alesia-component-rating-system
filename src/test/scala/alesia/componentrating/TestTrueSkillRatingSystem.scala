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

  /** Triplets: (Identifier, Strength, Supposed Ranking). */
  val rp = List(
    ("Component A", 5, 3),
    ("Component B", 3, 4),
    ("Component C", 7, 2),
    ("Component D", 10, 1))

  describe("Four Components are ranked, 6 combinations") {
    testSimpleRatingProblem(rp, Helper.linearCombination(rp, 2, false)) // 2 teams per game, no double occurrence of one team)
  }
  describe("Four Components are ranked, 5 combinations") {
    testSimpleRatingProblem(rp, List(List(rp(0), rp(1)), List(rp(2), rp(3)), List(rp(0), rp(3)), List(rp(1), rp(2)), List(rp(2), rp(0))))
  }
  describe("Four Components are ranked in 2 teams of 2") {
    testMultiRatingProblem(rp, Helper.linearCombination(Helper.linearCombination(rp, 2, false), 2, false))
  }
  // Here: Each team consists of only one component (Player)
  private def testSimpleRatingProblem(ratingProblem: List[Tuple3[String, Int, Int]], ganes: List[List[Tuple3[String, Int, Int]]]) {
    val tsrs = new TrueSkillRatingSystem()

    val combinations = ganes.map(l => l.sortBy(-_._2)).map(x => x.map(y => Set(y._1))) // Strength determines winning team
    combinations.foreach(tsrs.submitResults)

    val ranking = ratingProblem.sortBy(_._3).map(x => x._1)
    assertCorrectRanking(ranking, tsrs)
  }

  // Here: Each team consists of only multiple components (Player)
  private def testMultiRatingProblem(ratingProblem: List[Tuple3[String, Int, Int]], games: List[List[List[Tuple3[String, Int, Int]]]]) {
    val tsrs = new TrueSkillRatingSystem()

    val combinations = games.map(l => {
      l.sortBy( // Here sorting is used to put winning team first ...
        l.map(l2 => { (l2, regressive(l2.map(_._2.toDouble), 0.0, addDouble)) }).toMap // .. for that we need a hashmap Team->Teamstrength (Sum of Component Strength): Done here.
        ).reverse.map(x => x.toSet) // Teams get rearranged from List to Set here
    }).map(_.map(_.map(_._1))) // prune COmponent representation down to be only a String
    combinations.foreach(tsrs.submitResults)

    val ranking = ratingProblem.sortBy(_._3).map(x => x._1)
    assertCorrectRanking(ranking, tsrs)
  }

  private def assertCorrectRanking(l: List[String], tsrs: TrueSkillRatingSystem) {
    if (l.size == 1) return
    l.tail.foreach(x => {
      it("assertes the correct Ranking of " + l.head + " and " + x) {
        assertEquals("Correct Ranking", tsrs.compare(l.head, x), 1)
        assertEquals("Correct Ranking", tsrs.compare(x, l.head), -1)
      }
    })
    assertCorrectRanking(l.tail, tsrs)
  }

  def regressive[T](l: List[T], nul: T, function: (T, T) => T): T = {
    var result: T = nul
    l.foreach(x => result = function(result, x))
    result
  }

  def addDouble(a: Double, b: Double): Double = a + b
}