package alesia.componentrating.activeRanking.misc

import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests distance metrics.
 *
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class TestDistance extends FunSpec {

  val invDist = NumberOfInversionsDistance()

  val hammingDist = HammingDistance()

  val distances = List(invDist, hammingDist)

  describe("Distance Metrics") {
    it("cope with empty input") {
      distances.foreach(d => assert(0 === d.getDistance(List(), List())))
    }
    it("detect wrong ordering") {
      distances.foreach(d => assert(d.getDistance(List(1, 2), List(2, 1)) > 0))
    }
  }
}