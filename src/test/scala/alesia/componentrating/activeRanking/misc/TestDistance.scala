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

  val sampleList = List(1, 5, 7, 8, 4)

  val sampleListWrong4 = List(1, 5, 4, 7, 8)

  describe("Distance Metrics") {
    it("cope with empty input") {
      distances.foreach(d => assert(0 === d.getDistance(List(), List())))
    }
    it("detect wrong ordering") {
      distances.foreach(d => assert(d.getDistance(List(1, 2), List(2, 1)) > 0))
    }
  }

  describe("Distance: " + invDist) {
    it("correctly counts the inversions") {
      assert(0 === invDist.getDistance(sampleList, sampleList))
      assert(2 === invDist.getDistance(sampleList, sampleListWrong4))
      assert(6 === invDist.getDistance(sampleList, List(5, 4, 7, 8, 1)))
    }
  }

  describe("Distance: " + hammingDist) {
    it("correctly counts the number of errors") {
      assert(0 === hammingDist.getDistance(sampleList, sampleList))
      assert(3 === hammingDist.getDistance(sampleList, List(1, 5, 4, 7, 8)))
      assert(sampleList.length === hammingDist.getDistance(sampleList, sampleList.tail ::: List(sampleList.head)))
    }
  }
}