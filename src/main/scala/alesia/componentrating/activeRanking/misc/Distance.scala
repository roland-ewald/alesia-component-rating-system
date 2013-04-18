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
package alesia.componentrating.activeRanking.misc

import scala.collection.mutable.HashMap

/**
 * Distance: Function of two arrays (Lists) of "Things"
 * 		= 0 if both are the same
 * 		> 0 otherwise
 *
 * @author Jonathan Wienss
 * @author Roland Ewald
 */
trait Distance {

  /**
   * The arrays/lists are provided in HashMaps (Element) -> (Position)
   * This allows empty positions etc.
   */
  def getDistance[T](first: Map[T, Int], second: Map[T, Int]): Int

  /**
   * Creates the intersection of maps (by their keys).
   */
  def intersection[A, B](m1: Map[A, B], m2: Map[A, _]): Map[A, B] =
    m1.filter(x => m2.contains(x._1))

  def getDistance[T](first: List[T], second: List[T]): Int =
    getDistance(listToMap(first), listToMap(second))

  def getDistanceIntersected[T](first: Map[T, Int], second: Map[T, Int]): Int =
    getDistance(intersection(first, second), intersection(second, first))

  def getDistanceIntersected[T](first: List[T], second: List[T]): Int =
    getDistance(intersection(first, second), intersection(second, first))

  implicit def listToMap[T](list: List[T]): Map[T, Int] =
    list.zipWithIndex.toMap
}

/**
 * Hamming Distance ("Number of different digits")
 * 
 * This implementation is robust with regards to missing places. 
 * Example: compare the map[Element, place] 
 * 		(a -> 1), (b -> 2), (c -> 4) 
 * with
 * 		(a -> 1), (c -> 42), (b -> 4)
 * it yields a distance of 0. 
 * 
 * @author Jonathan Wienss
 * @author Roland Ewald
 */
case class HammingDistance() extends Distance {

  override def getDistance[T](first: Map[T, Int], second: Map[T, Int]): Int = {
    val pairsToMatch = first.toList.sortBy(_._2) zip second.toList.sortBy(_._2)
    pairsToMatch.filter(x => x._1._1 != x._2._1).length
  }
}

/**
 * Number of Inversions as distance
 * One inversion means that two places are interchanged
 * 
 * example List:[Element] where the element index indicates its place
 * 		(a,d,b,c) 
 * compared to
 * 		(a,b,c,d) 
 * yields a distance of 2.
 * 
 * @author Jonathan Wienss
 * @author Roland Ewald
 */
case class NumberOfInversionsDistance() extends Distance {

  override def getDistance[A](f: Map[A, Int], s: Map[A, Int]): Int = {
    for (
      e1 <- s.keys.toList;
      e2 <- s.keys.toList if s(e1) < s(e2)
    ) yield (if (f(e1) > f(e2)) 1 else 0)
  }.sum
}
