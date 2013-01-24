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