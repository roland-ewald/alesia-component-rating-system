package alesia.componentrating

import scala.collection.immutable.List

import alesia.componentrating.misc.Player
import alesia.componentrating.factorgraph.FactorGraphFactory
import alesia.componentrating.misc.AdvancedOptions
import alesia.componentrating.misc.NormalDist
import alesia.componentrating.misc.Team
import alesia.componentrating.misc.TrueSkillDefaultValues

/**
 * An implementation of the ComponentRatingSystem using TrueSkill.
 *
 * @author Jonathan Wienss
 * @author Michael Stein
 * @author Roland Ewald
 */
class TrueSkillRatingSystem(
  implicit dflt: TrueSkillDefaultValues = new TrueSkillDefaultValues(),
  advOpt: AdvancedOptions = new AdvancedOptions) extends ComponentRatingSystem {

  val updateUncertainyFactory = 1.5

  val knowledgeBase = scala.collection.mutable.Map[String, Player]()

  val useAdvancedOptions = false

  private[this] var advancedOptions = advOpt; // start with the provided implicit AdvancedOptions

  private[this] def defaultPlayer(id: String) = Player(dflt.defaultSkill.mean, dflt.defaultSkill.stDev, id)

  private[this] def getPlayer(id: String) = knowledgeBase.getOrElseUpdate(id, defaultPlayer(id))

  override def compare(c1: String, c2: String): Int =
    java.lang.Double.compare(getPlayer(c1).skill.mean, getPlayer(c2).skill.mean)

  override def componentUpdated(comp: String) = {
    val oldPlayer = getPlayer(comp)
    knowledgeBase(comp) = Player(oldPlayer.skill.mean, updateUncertainyFactory * oldPlayer.skill.stDev, comp)
  }

  override def submitResults(rankingForProblem: List[Set[String]]) = {
    val playerLists = rankingForProblem.map(t => t.map(getPlayer).toList)
    val playerTeams = playerLists.map(x => Team(x: _*))
    val updatedPlayers = FactorGraphFactory.factorGraphResults(playerTeams)(dflt = implicitly, advOpt = advancedOptions)
    var hm = scala.collection.mutable.HashMap[String, List[NormalDist]]()
    updatedPlayers.foreach(uP => updatedPlayers.foreach(uP2 => if (uP.id == (uP2.id)) hm += uP.id -> (uP2.skill.clone() :: hm.getOrElse(uP.id, List[NormalDist]()))))
    var newUpdatedPlayers = List[Player]()
    hm.keySet.foreach(key => {
      var i = new NormalDist(0, 0)
      // NOTE: "*" on NormalDist works similar to "+" on Int, Double, etc...
      hm(key).map(skill => i = i * (new NormalDist(skill.precision / hm(key).size, skill.precAdjustMean / hm(key).size)))
      newUpdatedPlayers = new Player(i, key) :: newUpdatedPlayers
    })

    newUpdatedPlayers.foreach(p => knowledgeBase += p.id -> p)
  }

  override def reset() = knowledgeBase.clear

  override def getPoints(comp: String) = getPlayer(comp).skill.mean

  override def getUncertainty(comp: String) = getPlayer(comp).skill.stDev

  override def setPartialPlayFactor(comp: String, factor: Double) = advancedOptions = advancedOptions.cloneAndSet(playerPartialPlayFactor = Tuple2(comp, factor))

  override def setWeightPartialPlayAgainstTeamSize(b: Boolean) = advancedOptions = advancedOptions.cloneAndSet(setPPBalancing = b)

  override def injectRating(comp: String, points: Double, uncertainty: Double) = knowledgeBase.put(comp, Player(points, uncertainty, comp))

}