package alesia.componentrating.activeRanking

/**
 * Stop conditions for active ranking.
 *
 * @author Jonathan Wienss
 */
trait StopCondition extends ActiveRankingComponent {
  /** Tests stop condition. */
  def apply(): Boolean
}

/**
 * A simple StopCondition based of the number of rounds.
 *
 * @author Jonathan Wienss
 */
class MaxRoundStopCondition(stopRound: Int = 10000, maxReplications: Int = 10) extends StopCondition {

  private[this] var aR: ActiveRanking = null

  override def register(aR: ActiveRanking) = this.aR = aR

  override def apply(): Boolean = aR.currentRound >= stopRound || aR.currentReplication >= maxReplications

  override def toString = "Stop Condition: stop at Round " + stopRound + ", max Replications: " + maxReplications
}