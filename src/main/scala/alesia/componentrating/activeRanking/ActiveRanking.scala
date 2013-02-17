package alesia.componentrating.activeRanking

import alesia.componentrating.misc.Helper
import alesia.componentrating.TrueSkillRatingSystem
import scala.util.Random
import alesia.componentrating.misc.TrueSkillDefaultValues
import alesia.componentrating.misc.AdvancedOptions
import alesia.componentrating.ComponentRatingSystem

/**
 * Active Ranking via a component rating system. Uses the component rating system and three additional components:
 *
 * The <code>RankingComparator</code> generates a ranking (e.g.: winner, 2nd place, ..., looser) for some match-up.
 * The comparisons are used for ranking the components.
 *
 * The <code>RatingLogger</code> logs the whole rating (not only single comparisons), per default nothing is logged.
 *
 * The <code>StopCondition</code> determines when the ActiveRanking should stop.
 *
 * @see MatchQuality
 * @see MatchUpSelector
 *
 * @author Jonathan Wienss
 *
 */
class ActiveRanking(
  val crs: ComponentRatingSystem,
  val dflt: TrueSkillDefaultValues = new TrueSkillDefaultValues,
  val advOpt: AdvancedOptions = new AdvancedOptions,
  val rng: Random,
  val stopCondition: StopCondition,
  val comparator: RankingComparator,
  val logger: RatingLogger = new LogNothing) {

  private[this] var round = 0
  private[this] var replication = 0

  def currentRound = round
  def currentReplication = replication

  private def init() = {
    round = 0
    replication = 0
    logger.register(this)
    stopCondition.register(this)
    comparator.register(this)
  }

  def execute() = {
    init()
    // iterate replications
    while (!stopCondition()) {
      // clears knowledge base
      crs.reset
      // iterate rounds (="games")
      while (!stopCondition()) {
        crs.submitResults(comparator())
        logger.log
        round = round + 1
      }
      // prepare for next replication:
      round = 0
      replication = replication + 1
      println("Replication Done")
    }
    logger.save
  }
}

/**
 * Components that rely on a reference to the active ranking.
 */
trait ActiveRankingComponent {

  /** Registers active ranking component to be logged. */
  def register(ar: ActiveRanking)
}



