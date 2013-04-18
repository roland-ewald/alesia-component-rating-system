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
package alesia.componentrating

/**
 * Interface with methods that work with Java types.
 * Should be equivalent to {@link ComponentRatingSystem}, apart from the method signatures.
 *
 * @see ComponentRatingSystem
 *
 * @author Roland Ewald
 */
trait IComponentRatingSystem extends java.util.Comparator[java.lang.String] {

  def submitResults(rankingForProblem: java.util.List[java.util.Set[java.lang.String]]): Unit

  def compare(c1: java.lang.String, c2: java.lang.String): Int

  def reset(): Unit

  def getComponentPoints(comp: java.lang.String): java.lang.Double

  def getComponentUncertainty(comp: java.lang.String): java.lang.Double

  def componentIsUpdated(comp: java.lang.String): Unit

  def compareComponents(components: java.util.List[java.lang.String]): java.util.List[java.lang.String]

  def injectRating(comp: java.lang.String, points: java.lang.Double, uncertainty: java.lang.Double): Unit

  def setWeightPartialPlayAgainstTeamSize(p: java.lang.Boolean): Unit

  def setPartialPlayFactor(comp: java.lang.String, factor: java.lang.Double)

}
