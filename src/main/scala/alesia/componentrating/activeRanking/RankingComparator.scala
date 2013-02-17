package alesia.componentrating.activeRanking

/**
 * Generates a ranking (e.g.: winner, 2nd place, ..., looser) for each selected match-up.
 * The match-ups are selected by a <code>MatchUpSelector</code>,
 * which in turn is based on <code>MatchQuality</code>.
 *
 * @see MatchUpSelector
 *
 * @author Jonathan Wienss
 */
abstract class RankingComparator(useVirtualPlayerTeamWeighting: Boolean = false) extends ActiveRankingComponent {

  val components: Set[String] = getComponents

  /** Get available components. */
  def getComponents: Set[String]

  /** Generate a comparison of component teams. */
  def apply(): List[Set[String]]
}