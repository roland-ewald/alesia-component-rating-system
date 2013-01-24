package alesia.componentrating.activeRanking

import alesia.componentrating.misc.Helper
import alesia.componentrating.TrueSkillRatingSystem
import scala.util.Random
import alesia.componentrating.misc.TrueSkillDefaultValues
import alesia.componentrating.misc.AdvancedOptions

/**
 * Active Ranking via TrueSkill.
 * Uses the Passive Rating (ComponentRatingSystem) with 2 new Components:
 * 		MatchUpSelector - Selects the best possible MatchUp of the Components and submits it
 * 				(Uses MatchQuality to determine, what is "best MatchUp")
 * 		Comparator - For each submitted MatchUp generates a comparison (one could say, winner, 2nd place, ..., looser).
 * 				The Comparisons are used for Ranking
 * 		Logger - (semi optional) Loggs the whole ranking (not only single comparisons)
 * 				Use a blank implementation if not needed.
 * 		StopCondition - used to stop the whole Ranking (which means, no more ratings / comparisons)
 *
 * @author Jonathan Wienss
 *
 */
class ActiveRanking(
  val dflt: TrueSkillDefaultValues = new TrueSkillDefaultValues,
  val advOpt: AdvancedOptions = new AdvancedOptions,
  val rng: Random,
  val stopCondition: StopCondition,
  val comparator: TSComparator,
  logger: TSLogger) {

  private[this] var round = 0
  def currentRound = round
  private[this] var replication = 0
  def currentReplication = replication

  val tsrs = new TrueSkillRatingSystem()(dflt, advOpt)

  private def init() = {
    round = 0
    replication = 0
    logger.register(this)
    stopCondition.register(this)
    comparator.register(this)
  }

  def execute() = {
    init()
    while (!stopCondition()) { // iterate replications
      tsrs.reset // clears the knowledgebase
      while (!stopCondition()) { // iterate rounds (="games")
        tsrs.submitResults(comparator())

        logger.log

        round = round + 1
      }

      // prepare for next replication:
      round = 0
      replication = replication + 1
      System.out.println("-Replication-")
    }
    logger.save // writes results to file
  }
}

/**
 * Abstract classes
 */
abstract class TSComparator(useVirtualPlayerTeamWeighting: Boolean = false) {
  val components: Set[String] = getComponents // keeps track of all components
  def getComponents: Set[String]
  def register(aR: ActiveRanking)
  def apply(): List[Set[String]] // compares simulators(or "teams") 
}

abstract class TSLogger() {
  def register(aR: ActiveRanking)
  def log() // reads current state of the experiment
  def save() // save results to file
}

abstract class StopCondition() {
  def register(aR: ActiveRanking)
  def apply(): Boolean // test on stop condition
}



