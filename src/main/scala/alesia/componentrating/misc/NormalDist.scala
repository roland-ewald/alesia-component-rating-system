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

import scala.math._
import org.apache.commons.math3.special.Erf

/**
 * A randomly (normal) distributed variable. Defined by Precision = 1 / Variance and Precision-Adjusted-Mean = Mean * Precision.
 *
 * @author Jonathan Wienss
 *
 * @param precision precision = 1 / Variance
 * @param precAdjustMean = Precision * Mean
 *
 */
class NormalDist(val precision: Double, val precAdjustMean: Double) {

  /**
   * The mean.
   */
  def mean: Double = precAdjustMean / precision

  /**
   * The standard deviation.
   */
  def stDev: Double = scala.math.sqrt(variance)

  /**
   * The variance.
   */
  def variance: Double = 1.0 / precision

  /**
   * The Normalization Constant. Not used?
   */
  def normalizationConst: Double = 1.0 / (scala.math.sqrt(2.0 * scala.math.Pi) * stDev)

  /**
   * Multiplication of two Values. 
   */
  def *(other: NormalDist): NormalDist = new NormalDist(precision + other.precision, precAdjustMean + other.precAdjustMean)

  /**
   * Division of two Values. 
   */

  def /(other: NormalDist): NormalDist = new NormalDist(precision - other.precision, precAdjustMean - other.precAdjustMean)

  /**
   * A difference Metric. Used ONLY for calculating the amount of update in approximative approach. It is a Metric of the amount of Update a variable has experienced between two runs.
   */
  def -(other: NormalDist): Double =
    max(abs(precAdjustMean - other.precAdjustMean), sqrt(abs(precision - other.precision)))

  /**
   * cumulative propability of the standard normal distribution
   *
   * Only use for x>0 because of point reflecion at (0, 0.5)
   * Uses James Errorfunction
   */
  def cumulativePropability(x: Double): Double = 0.5 + 0.5 * Erf.erf(x / scala.math.sqrt(2))

  /**
   * Density function of this normal distribution
   */
  def density(x: Double) = 1.0 / (stDev * sqrt(2.0 * Pi)) * exp(-0.5 * pow((x - mean) / stDev, 2.0))

  override def toString: String = return "Mean: " + mean + ", Sigma: " + stDev

  override def clone() = new NormalDist(precision, precAdjustMean)
}
