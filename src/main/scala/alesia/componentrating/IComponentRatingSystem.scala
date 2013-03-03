package alesia.componentrating

/**
 * Interface with methods that work with Java types.
 *
 * @see ComponentRatingSystem
 *
 * @author Roland Ewald
 */
trait IComponentRatingSystem extends java.util.Comparator[java.lang.String] {

  def injectRating(comp: java.lang.String, points: java.lang.Double, uncertainty: java.lang.Double): Unit

  def setWeightPartialPlayAgainstTeamSize(p: java.lang.Boolean): Unit

  def setPartialPlayFactor(comp: java.lang.String, factor: java.lang.Double)

}