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
abstract class Distance {

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
 * Hamming Distance ("Number of wrong digits").
 *
 * Implemented to be invariant vs missing ranks (and thus intersections from above which very likely have missing ranks).
 *
 * @author Jonathan Wienss
 *
 * missing rank: (a -> 1), (b -> 2), (c -> 4)  [<- should be rank 3]
 *
 */
case class HammingDistance() extends Distance {

  def getDistance[T](first: Map[T, Int], second: Map[T, Int]): Int = {
    var tFirst: HashMap[T, Int] = new HashMap[T, Int]() ++ first
    var tSecond: HashMap[T, Int] = new HashMap[T, Int]() ++ second
    var dist = 0

    first.keySet.foreach(key => {
      if (getMin(tFirst) != getMin(tSecond)) dist = dist + 1
      tFirst.remove(getMin(tFirst))
      tSecond.remove(getMin(tSecond))
    })

    return dist
  }

  def getMin[T](map: HashMap[T, Int]): T = {
    var candidate = map.keySet.head
    map.keySet.foreach(key => if (map(key) < map(candidate)) candidate = key)
    candidate
  }
}

/**
 * Number of Inversions as distance
 * One Inversion: Two Places are interchanged
 * Number of Inversions: Min steps of interchanging of two places to reach correct ordering
 *
 * @author Jonathan Wienss
 * @author Roland Ewald
 */
case class NumberOfInversionsDistance() extends Distance {

  override def getDistance[A](f: Map[A, Int], s: Map[A, Int]): Int = {
    for (
      e1 <- s.keys.toList;
      e2 <- s.keys.toList if f(e1) < f(e2)
    ) yield (if (s(e1) > s(e2)) 1 else 0)
  }.sum
}