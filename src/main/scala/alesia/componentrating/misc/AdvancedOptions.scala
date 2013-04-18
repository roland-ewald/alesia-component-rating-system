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
package alesia.componentrating.misc

import scala.collection.immutable.HashMap

/**
 * Other advanced options for TrueSkill implementations.
 * Eg Team Size Balancing via Partial Play or Virtual Players
 *
 * @author Jonathan Wienss
 *
 */
class AdvancedOptions(
  val usePPBalancing: Boolean = false,
  val useVPBalancing: Boolean = false,
  val playerPartialPlay: Map[String, Double] = HashMap[String, Double]()) {

  def getPlayerPartialPlay(player: String): Double = playerPartialPlay.getOrElse(player, 1.0)

  def cloneAndSet(setPPBalancing: Boolean = usePPBalancing,
    setVPBalancing: Boolean = useVPBalancing,
    playerPartialPlayFactor: Tuple2[String, Double] = null): AdvancedOptions = new AdvancedOptions(
    usePPBalancing = setPPBalancing,
    useVPBalancing = setVPBalancing,
    playerPartialPlay = if (playerPartialPlayFactor == null) playerPartialPlay else playerPartialPlay + playerPartialPlayFactor)
}
