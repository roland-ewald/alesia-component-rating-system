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
