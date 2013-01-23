package alesia.componentrating.activeRanking

import scala.collection.mutable.HashMap

import alesia.componentrating.activeRanking.misc.Distance
import alesia.componentrating.misc.Helper
import alesia.componentrating.misc.Serializer

/**
 * FileLogger saves  the rating results to file.
 *
 * @author Jonathan Wienss
 *
 */
class TSFileLogger(targetFile: String = "unknown.xml", realRankingFile: String = "RealRanking filename missing.xml", distances: List[Distance], saveEveryXRounds: Int = 10)
  extends TSLogger {
  val data = new Round()

  private[this] var aR: ActiveRanking = null
  def register(aR: ActiveRanking) = TSFileLogger.this.aR = aR

  val realRankingHM = HashMap[String, Int]()
  class Container(val stuff: List[(java.lang.String, Int)]) extends Serializable {}
  (Serializer.fromFile(realRankingFile): List[(java.lang.String, Int)]).foreach(tupel => realRankingHM += tupel._1 -> tupel._2)

  def log(): Unit = {
    val replication = aR.currentReplication
    val round = aR.currentRound

    if (!(round % saveEveryXRounds == 0)) return
    val playerBeliefMean = HashMap[String, Double]()
    aR.comparator.components.foreach(p => playerBeliefMean += p -> Helper.salt(aR.tsrs.getPoints(p), aR.rng))
    val playerBeliefSigma = HashMap[String, Double]()
    aR.comparator.components.foreach(p => playerBeliefSigma += p -> aR.tsrs.getUncertainty(p))
    val beliefRanking = playerBeliefMean.keySet.toList.sortBy(-1 * playerBeliefMean(_)) // NOTE: sort sorts from smallest to greatest
    val distance = (distances zip distances.map(m => m.getDistanceIntersected(realRankingHM, m.listToHM(beliefRanking)))).toMap

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