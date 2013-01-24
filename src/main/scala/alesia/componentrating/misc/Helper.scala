package alesia.componentrating.misc

import scala.collection.mutable.HashMap
import scala.collection.immutable.List
import scala.util.Random

/**
 * Contains functions that are used often and under different context throughout the code
 *
 * @author Jonathan Wienss
 */
object Helper {

  /**
   * Yields all combinations of elements of a list up to a given size.
   *
   * @param list the elements are drawn from this list
   * @param size maximum size of the result
   * @param repeat weather an element can be multiple times in the result
   */
  def linearCombination[T](list: List[T], size: Int, repeat: Boolean): List[List[T]] = {
    if (!repeat && size > list.size) return List[List[T]]()
    if (size == 1) {
      return list.map(x => x :: List())
    }

    // All Combinations with right.head
    var combinations = List[List[T]]()
    linearCombination(list.tail, size - 1, repeat).foreach(candidateCombination => {
      combinations = if (repeat || candidateCombination.forall(elem => list.head != elem))
        (list.head :: candidateCombination) :: combinations else combinations
    })
    // Now add Teams without right.head
    combinations = linearCombination(list.tail, size, repeat) ::: combinations

    return combinations
  }

  /**
   * Counters the influence of different teamsizes on teamstrength via the use of virtual players in smaller teams
   * In the result all teams have the same amount of players, but the ratio of the players inside a team
   * towards each other is maintained.
   */
  def virtualPlayerTeamWeighting(rankingForProblem: List[Set[String]]): List[Set[String]] = {
    var leastCommonMultiplier: List[Int] = List()
    rankingForProblem.foreach(team => {
      val candidate = team.size

      // delete all multipliers, which divide candidate to 0
      var temp = leastCommonMultiplier
      leastCommonMultiplier = List()
      while (temp.size != 0) {
        if (candidate % temp.head == 0) null;
        else leastCommonMultiplier = temp.head :: leastCommonMultiplier

        temp = temp.tail
      }
      leastCommonMultiplier = candidate :: leastCommonMultiplier
    })

    var lCM = 1
    leastCommonMultiplier.foreach(n => lCM = lCM * n)
    // now each team needs to be added virtual players up to least common multiplier
    var newProblem: List[Set[String]] = List()
    rankingForProblem.foreach(oldTeam => {
      var newTeam = List[String]()
      oldTeam.foreach(player => {
        for (i <- 1 to lCM / oldTeam.size) {
          newTeam = player :: newTeam
        }
      })
      newProblem = newTeam.toSet :: newProblem
    })

    return newProblem.reverse
  }
  /*
   * 
   * --------------------------- Based on Random Values: ------------------------------------
   * 
   */
  /**
   * Salts a provided value
   */
  def salt(value: Double, rng: Random): Double = {
    value + (rng.nextDouble - 0.5) * 0.000001
  }

  /**
   * Choose randomly from pool of things
   */
  def chooseRand[A](stuff: List[A], rng: Random): A = {
    return stuff((rng.nextDouble * stuff.size).toInt)
  }

  /**
   * Choose randomly - and weighted! - from pool
   */
  def chooseRandWeighted[A](stuff: scala.collection.Iterable[A], weights: Map[A, Double], rng: Random): A = {
    var sumWeights = 0.0
    weights.values.foreach(x => sumWeights = sumWeights + x)

    var stuff2 = stuff
    var x = rng.nextDouble
    while (x >= weights(stuff2.head) / sumWeights) {
      x = x - weights(stuff2.head) / sumWeights
      stuff2 = stuff2.tail
    }
    return stuff2.head
  }
}