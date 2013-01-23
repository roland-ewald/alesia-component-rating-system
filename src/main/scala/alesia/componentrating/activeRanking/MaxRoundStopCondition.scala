package alesia.componentrating.activeRanking

/**
 * A simple StopCondition based of the number of rounds.
 * 
 * @author Jonathan Wienss
 *
 */
class MaxRoundStopCondition(stopRound: Int = 10000, maxReplications: Int = 10) extends StopCondition {
	private[this] var aR: ActiveRanking = null
    def register(aR: ActiveRanking) = this.aR = aR
    
	def apply(): Boolean = aR.currentRound >= stopRound || aR.currentReplication >= maxReplications 
	
	override def toString = "Stop Condition: stop at Round "+stopRound+", max Replications: "+maxReplications
}