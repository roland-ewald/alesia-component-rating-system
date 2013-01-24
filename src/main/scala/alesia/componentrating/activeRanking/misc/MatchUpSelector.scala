package alesia.componentrating.activeRanking.misc

import scala.collection.mutable.HashMap
import scala.util.Random
import alesia.componentrating.misc.Helper

/**
 * matchUpSelector choses the best MatchUp according to provided MatchQuality
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