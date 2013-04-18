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
package alesia.componentrating.activeRanking

import scala.collection.mutable.HashMap

import alesia.componentrating.activeRanking.misc.Distance
import alesia.componentrating.misc.Helper
import alesia.componentrating.misc.Serializer

/**
 * Saves the rating results.
 *
 * @author Jonathan Wienss
 *
 */
trait RatingLogger extends ActiveRankingComponent {
  /** Stores current state of the experiment. */
  def log()
  /** Save results. */
  def save()
}

/**
 * Default implementation (nothing is logged).
 */
class LogNothing extends RatingLogger {
  override def register(aR: ActiveRanking) = {}
  override def log() = {}
  override def save() = {}
}

/**
 * Saves the rating results to file.
 */
class FileRatingLogger(
  targetFile: String = "unknown.xml",
  realRankingFile: String = "RealRanking filename missing.xml",
  distances: List[Distance],
  saveEveryXRounds: Int = 10) extends RatingLogger {

  class Container(val stuff: List[(java.lang.String, Int)]) extends Serializable {}

  val data = new Round

  private[this] var aR: ActiveRanking = null

  def register(aR: ActiveRanking) = this.aR = aR

  val realRankingHM = HashMap[String, Int]()

  (Serializer.fromFile(realRankingFile): List[(java.lang.String, Int)]).foreach(x => realRankingHM += x._1 -> x._2)

  def log(): Unit = {
    val replication = aR.currentReplication
    val round = aR.currentRound

    if (!(round % saveEveryXRounds == 0)) return
    val playerBeliefMean = HashMap[String, Double]()
    aR.comparator.components.foreach(p => playerBeliefMean += p -> Helper.salt(aR.crs.getPoints(p), aR.rng))
    val playerBeliefSigma = HashMap[String, Double]()
    aR.comparator.components.foreach(p => playerBeliefSigma += p -> aR.crs.getUncertainty(p))
    val beliefRanking = playerBeliefMean.keySet.toList.sortBy(-1 * playerBeliefMean(_)) // NOTE: sort sorts from smallest to greatest
    val distance = (distances zip distances.map(m => m.getDistanceIntersected(realRankingHM.toMap, m.listToMap(beliefRanking)))).toMap

    data.setRound(replication, round, distance)
  }

  def save = {
    data.addGlobalInfo("Stop Condition", aR.stopCondition.toString())
    data.addGlobalInfo("TrueSkill Default Values", aR.dflt.toString)
    data.addGlobalInfo("Real Ranking", realRankingHM.keySet.toList.sortBy(realRankingHM).mkString)

    // save
    Serializer.toFile(targetFile, data)
  }
}

/**
 * Contains the information about one experiment with active ranking.
 *
 * @author Jonathan Wienss
 *
 */
class Round extends Serializable {

  // Replication -> (Round -> (Distance -> Int))
  val info = HashMap[Int, HashMap[Int, scala.collection.immutable.Map[Distance, Int]]]()
  val globalInfo = HashMap[String, String]()

  def setRound(replication: Int, round: Int, information: scala.collection.immutable.Map[Distance, Int]) = {
    val hm = info.getOrElse(replication, HashMap[Int, scala.collection.immutable.Map[Distance, Int]]())
    hm += round -> information
    info += replication -> hm
  }

  def addGlobalInfo(s1: String, s2: String) = globalInfo.put(s1, s2)
}
