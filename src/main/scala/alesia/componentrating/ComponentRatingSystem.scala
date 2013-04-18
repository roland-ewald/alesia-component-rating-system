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

import scala.collection.mutable.ListBuffer
import java.util.ArrayList

/**
 * General interface for component rating system.
 * This is intended to be used in passive way, i.e. submitting results via
 * {@link ComponentRatingSystem#submitResults} incrementally improves the ranking that eventually
 * reflect the overall performance of the components. Each component is associated with a (real) value of
 * 'points' that estimates the expected strength/suitability/etc. of the components in the relation to the others.
 * Additionally, each component is associated with an uncertainty (also given a real number), which refers
 * to the 'points' estimate of that component.
 *
 * The ranking can be used via the Comparator interface.
 *
 * @see java.util.Comparator
 *
 * @author Jonathan Wienss
 * @author Michael Stein
 * @author Roland Ewald
 */
trait ComponentRatingSystem extends IComponentRatingSystem {

  /**
   * Submits new results to knowledge base.
   * This adds new observation and triggers a new ranking (update) of the components in this set of results.
   * @param rankingForProblem the teams in the order of the ranking (best first)
   */
  def submitResults(rankingForProblem: List[Set[String]]): Unit

  /**
   * Compares two components based on their current ranking.
   * @param c1 id of component 1
   * @param c2 id of component 2
   * @see java.util.Comparator
   * @return number smaller than 0 iff c1 < c2, greater than 0 iff (c2 > c1), 0 otherwise
   */
  def compare(c1: String, c2: String): Int

  /**
   * Deletes the knowledge base.
   */
  def reset(): Unit

  /**
   * @param comp the component id
   * @return the expected points of the component
   */
  def getPoints(comp: String): Double

  /**
   * @param comp the component id
   * @return the uncertainty of the 'points' estimation of this component
   */
  def getUncertainty(comp: String): Double

  /**
   * Call this to indicate that a component has been changed.
   * The uncertainty of the 'points' estimation will be adjusted (this is implementation-specific).
   * @param comp the component id
   */
  def componentUpdated(comp: String): Unit

  /**
   * Ranks several components.
   * @param components the component ids
   * @return ranking of components (best first)
   */
  def compareComponents(components: String*): List[String] = components.toList.sortWith((c1, c2) => compare(c1, c2) < 0)

  /**
   * Set the Partial Play factor for a certain component. Standard is 1.0 which is used if a factor for a player is not set.
   * @param factor The Partial Play factor for this component. Standard is 1.0. E.g.: use 0.5 to achieve 50% Partial Play.
   */
  def setPartialPlayFactor(comp: String, factor: Double)

  /**
   * If True, the Partial Play factor of each component in a compound algorithm is weighted against the number of components
   * in that algorithm in such a way, that the sum of all pre-factors of components of that algorithm is 1.
   * In other words the number of other components of an algorithm is no longer affecting the ranking of a component.
   */
  def setWeightPartialPlayAgainstTeamSize(b: Boolean)

  /**
   * Inject the rating (points and uncertainty) of one component into the Rating System. The new rating overwrites any old
   * rating, this component might have had in the system before
   *
   */
  def injectRating(comp: String, points: Double, uncertainty: Double): Unit

  // Methods for easier access from Java: 

  override def submitResults(rankingForProblem: java.util.List[java.util.Set[java.lang.String]]): Unit = {
    val ranking = ListBuffer[Set[String]]()
    for (i <- 0 until rankingForProblem.size()) {
      val oldSet = rankingForProblem.get(i)
      val newSet = ListBuffer[String]()
      val it = oldSet.iterator()
      while (it.hasNext) {
        newSet += it.next()
      }
      ranking += newSet.toSet
    }
    submitResults(ranking.toList)
  }

  override def getComponentPoints(comp: java.lang.String): java.lang.Double = getPoints(comp)

  override def getComponentUncertainty(comp: java.lang.String): java.lang.Double = getUncertainty(comp)

  override def componentIsUpdated(comp: java.lang.String): Unit = componentUpdated(comp)

  override def compareComponents(components: java.util.List[java.lang.String]): java.util.List[java.lang.String] = {
    val newList = ListBuffer[java.lang.String]()
    val it = components.iterator()
    while (it.hasNext)
      newList += it.next()

    val rv = new ArrayList[java.lang.String]()
    val results = compareComponents(newList: _*)
    results.foreach(v => rv.add(v))
    rv
  }

  override def setPartialPlayFactor(comp: java.lang.String, factor: java.lang.Double) = setPartialPlayFactor(comp, factor)

  override def setWeightPartialPlayAgainstTeamSize(b: java.lang.Boolean) = setWeightPartialPlayAgainstTeamSize(b)

  override def injectRating(comp: String, points: java.lang.Double, uncertainty: java.lang.Double) = injectRating(comp, points, uncertainty)

}
