package alesia.componentrating.factorgraph

import scala.collection.mutable.HashMap

import alesia.componentrating.misc.AdvancedOptions
import alesia.componentrating.misc.NormalDist
import alesia.componentrating.misc.Player
import alesia.componentrating.misc.Team
import alesia.componentrating.misc.TrueSkillDefaultValues

/**
 * Builds the FactorGraph for TrueSkill RankCalculator. Use: FactorGraph.factorgraphResults(teams)
 *
 * @author: Jonathan Wienss
 *
 */
object FactorGraphFactory {

  /**
   * Returns The Results of the RankCalculator for a given Game.
   *
   * @param teams The List of teams in this Game. The Player Instances in each Team are NOT manipulated (see results).
   * @return Returns a List of new Instances of Player. Each Instance shares the ID with one Player from teams (see parameters) and holds the new skill of this player. When using, set the skill in your knowledgebase yourself (is done in {@see TrueSkillControl}).
   */
  def factorGraphResults(teams: List[Team])(
    implicit dflt: TrueSkillDefaultValues = new TrueSkillDefaultValues,
    advOpt: AdvancedOptions = new AdvancedOptions): List[Player] = {
    var resulters = List[Resulter]()
    val scheduler = new Scheduler

    // teams is expected to be sorted from winner to loser
    var teamPerformanceVariables = List[Variable]();
    var teamnames = List[String]()
    val numberOfPLayersPerTeam = new HashMap[Variable, Int]()
    (if (advOpt.useVPBalancing) virtualPlayerTeamWeighting(teams) else teams).foreach(t => {
      var playerPerformanceVariables = List[Variable]();
      val playerPrefactors = HashMap[Variable, Double]()
      t.players.foreach(p => {
        val tupel = buildPlayer(p, scheduler);
        playerPerformanceVariables = tupel._1 :: playerPerformanceVariables
        resulters = tupel._2 :: resulters

        // Partial Play
        playerPrefactors += tupel._1 -> advOpt.getPlayerPartialPlay(p.id)
      })
      playerPerformanceVariables = playerPerformanceVariables.reverse
      val tupel = buildTeam(playerPerformanceVariables, t.players, playerPrefactors, scheduler)
      teamPerformanceVariables = tupel._1 :: teamPerformanceVariables
      teamnames = tupel._2 :: teamnames
      numberOfPLayersPerTeam += tupel._1 -> tupel._3
    })
    teamPerformanceVariables = teamPerformanceVariables.reverse
    teamnames = teamnames.reverse
    buildResults(teamPerformanceVariables, teamnames, numberOfPLayersPerTeam, scheduler)

    if (teams.size == 2) scheduler.scheduleTwoTeams
    else scheduler.scheduleApprox

    var results = List[Player]()
    resulters.foreach(r => results = r.execute :: results)
    return results
  }

  /**
   * Constructs Factor Graph for a Player
   *
   * @param p The Player for this FactorGraph part
   * @param scheduler reference to the Scheduler
   *
   * @return Node to connect with TeamFactor, Resulter for this Player
   */
  private[this] def buildPlayer(p: Player, scheduler: Scheduler)(implicit dflt: TrueSkillDefaultValues): (Variable, Resulter) = {
    // Create factors and variables:
    val fPrior = new FactorPrior("Prior " + p, p)
    val vSkill = new Variable("Skill " + p, new NormalDist(0, 0))
    val fPerfo = new FactorPerformance("Performance " + p, p)
    val vPerfo = new Variable("Performance " + p, new NormalDist(0, 0))

    // Link the Nodes:
    fPrior.neighborDown = vSkill
    fPerfo.neighborsUp = vSkill :: fPerfo.neighborsUp
    fPerfo.neighborDown = vPerfo

    // Resulters will give us access to the Results in the end:
    val resulter = new Resulter(vSkill, p.id)

    // Create and Register the Steppers in the Scheduler.
    scheduler.stepDown1 = new Stepper(fPrior, vSkill) :: scheduler.stepDown1
    scheduler.stepDown2 = new Stepper(fPerfo, vPerfo) :: scheduler.stepDown2
    scheduler.stepUp4 = new Stepper(fPerfo, vSkill) :: scheduler.stepUp4

    // The Return values will be used to extend the factor graph further "down" following the graphic in [HERRBICH]
    return new Tuple2(vPerfo, resulter)
  }

  /**
   * Constructs Factor Graph for a Team
   *
   * @param vs List of PlayerPerformanceVariables (Each Player in this Team).
   * @param players not used atm, may be used for debugging later (for building a teamname)
   * @param scheduler reference to the scheduler
   * @param partialPlay the share of each player (looked up via playerperformanceVariable) of the teamresult ("Patial Play"). 1.0 is standard.
   *
   * @return PerformanceVariable for the Team
   */
  private[this] def buildTeam(vs: List[Variable], players: List[Player], partialPlay: HashMap[Variable, Double], scheduler: Scheduler)(implicit dflt: TrueSkillDefaultValues, advOpt: AdvancedOptions = new AdvancedOptions): (Variable, String, Int) = {

    // Create factors and variables:
    val fTeam = new FactorSum("TeamF") // Team = Sum of its Components

    // Partial Play: sum up all prefactors
    var prefactorSumForThisTeam = 0.0
    vs.foreach(v => prefactorSumForThisTeam = prefactorSumForThisTeam + partialPlay(v))

    // Now link all Player Variables to the Teamfactor:
    var numberOfPlayers = 0
    vs.foreach(v => {
      fTeam.neighborsUp = v :: fTeam.neighborsUp
      // prefactors implement partial play
      if (advOpt.usePPBalancing) {
        fTeam.prefactors += v -> partialPlay(v) / vs.size // Partial Play Team Weighting used 
      } else {
        fTeam.prefactors += v -> partialPlay(v)
      }

      numberOfPlayers = numberOfPlayers + 1

      scheduler.stepUp3 = new Stepper(fTeam, v) :: scheduler.stepUp3
    })

    // Teamname could be used for debugging later:
    var teamname = ""
    players.foreach(p => teamname = p + teamname)

    // Create and Link Team Performance Variable:
    val vTeam = new Variable("TeamPerf " + teamname, new NormalDist(0, 0))
    fTeam.neighborDown = vTeam
    fTeam.prefactors += vTeam -> -1.0 // Always -1.0

    // Register Stepper:
    scheduler.stepDown3 = new Stepper(fTeam, vTeam) :: scheduler.stepDown3

    return Tuple3(vTeam, teamname, numberOfPlayers)
  }

  /**
   * Constructs the lower Part of the Factor Graph
   *
   * Takes Team Performance variables - SORTED FROM 1st Ranking team downwards!!!
   * @param vs List of TeamPerformanceVariables - sorted from 1st Rank Team to last Rank Team!
   * @param teamnames teamnames for debugging
   * @param numberofPlayersPerTeam amount of players in each team for the Gaussian Comparison Win Factor ({@see Factor_GausCompWin}). (HashMap: TeamPerformanceVariable -> numberofplayers)
   * @param scheduler reference to the scheduler
   *
   * Returns Approximation Starters
   */
  private[this] def buildResults(vs: List[Variable], teamnames: List[String], numberOfPLayersPerTeam: HashMap[Variable, Int], scheduler: Scheduler)(implicit dflt: TrueSkillDefaultValues): Unit = {
    // For each neighboring 2 Teams there must be one comparison part in the factorgraph. E.g. for 4 Teams there are 3 comparison parts.
    // Here we work from Left to Right, thus calling the Teams "Left" and "Right".
    var tn2 = teamnames
    var vs2: List[Variable] = vs
    var vLEFT = vs.head
    var numberOfPlayersTeamLeft: Int = 0
    var numberOfPlayersTeamRight: Int = 0
    vs2 = vs2.tail
    var counter = 1
    vs2.foreach(vRIGHT => {
      numberOfPlayersTeamLeft = numberOfPLayersPerTeam(vLEFT)
      numberOfPlayersTeamRight = numberOfPLayersPerTeam(vRIGHT)
      val fSum = new FactorSum("Sum" + tn2.head + tn2.tail.head)
      tn2 = tn2.tail

      // Whinning Team is always on the left
      // Using the Formula from [HERBRICH07]:
      fSum.neighborsUp = vLEFT :: fSum.neighborsUp // Sum Factor - Performance Variable (Team 1) -> The Winner
      fSum.prefactors += vLEFT -> 1.0
      fSum.neighborsUp = vRIGHT :: fSum.neighborsUp // Sum Factor - Performance Variable (Team 2) -> The Loser
      fSum.prefactors += vRIGHT -> -1.0

      // Register Steppers:
      scheduler.stepUp2 = new Stepper(fSum, vLEFT) :: scheduler.stepUp2
      scheduler.stepUp2 = new Stepper(fSum, vRIGHT) :: scheduler.stepUp2

      // Difference variable and linking:
      val vDIFF = new Variable("Diffference", new NormalDist(0, 0))
      fSum.neighborDown = vDIFF // Sum Factor - Difference Variable
      fSum.prefactors += vDIFF -> -1.0

      // Gaussian comparison Win Factor, linking:
      val fGCW = new FactorGaussianComparisonWin("GaussianCWin 1+2", numberOfPlayersTeamLeft + numberOfPlayersTeamRight)
      fGCW.neighborsUp = vDIFF :: fGCW.neighborsUp

      // Steppers for the above nodes:
      scheduler.stepDown4 = new Stepper(fSum, vDIFF) :: scheduler.stepDown4
      scheduler.stepUp1 = new Stepper(fGCW, vDIFF) :: scheduler.stepUp1

      // Approximative Approach is done in two directions: from left to right and afterwards from right to left
      // For Detail, refer to graphic in [HERBRICH07]
      // Approximation Step Right: not for the last team, because this is already covert by step left:
      if (counter != vs.tail.size) scheduler.stepApRight = new StepperApproximationGetter(vRIGHT, scheduler) :: new Stepper(fSum, vRIGHT) :: new StepperApproximationGetter(vDIFF, scheduler) :: new Stepper(fGCW, vDIFF) :: new Stepper(fSum, vDIFF) :: scheduler.stepApRight
      // Approximation Step Left: not for the first team, because this is covert by step right
      if (counter != 1) scheduler.stepApLeft = new Stepper(fSum, vDIFF) :: new Stepper(fGCW, vDIFF) :: new StepperApproximationGetter(vDIFF, scheduler) :: new Stepper(fSum, vLEFT) :: new StepperApproximationGetter(vLEFT, scheduler) :: scheduler.stepApLeft

      // iterate:
      vLEFT = vRIGHT
      counter = counter + 1
    })

    // delete the borders (far right and far left)
    // Necessary, because those parts are covered by non-approximative approach and are not part of the shortest path between two nodes, that need approximation
    // Again, refer to the Graphic in [HERBRICH07]
    scheduler.stepApRight = scheduler.stepApRight.reverse
    scheduler.stepApLeft = (scheduler.stepApLeft)
    return
  }

  /**
   * Counters different teamsizes via the use of virtual players in smaller teams
   * In the result all teams have the same amount of (probably virtual) players, but the ratio of the players insie a team
   * towards each other is maintained.
   *
   * This should negate the influence of teamsizes on the result of the players.
   */
  private def virtualPlayerTeamWeighting(rankingForProblem: List[Team]): List[Team] = {
    var leastCommonMultiplier: List[Int] = List()
    rankingForProblem.foreach(team => {
      val candidate = team.players.size

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
    var newProblem: List[Team] = List()
    rankingForProblem.foreach(oldTeam => {
      var newTeam = Team()
      oldTeam.players.foreach(player => {
        for (i <- 1 to lCM / oldTeam.players.size) {
          newTeam = new Team(player :: newTeam.players)
        }
      })
      newProblem = newTeam :: newProblem
    })

    return newProblem.reverse
  }
}

/**
 * Only use in FactorFraphFactory. Gets the Results for a player. Agent design pattern.
 *
 * @param v player performance variable
 * @param id ID for the player identification
 *
 * @result a Player instance, which holds the skill for the player with the ID id
 */
class Resulter(v: Variable, id: String) {
  def execute: Player = new Player(v.value.clone(), id)
}

/**
 * Only in FactorGraphFactory. Executes one small step for the scheduler. Agent Design pattern.
 *
 * @param f a Factor to execute.
 * @param v the variable, the Factor should update
 *
 */
class Stepper(f: Factor, v: Variable) {
  def execute { f.update(v) }
}

/**
 * ONly in FactorGraphFactory. This gets the Variable values for the approximative approach.
 *
 * ...
 */
class StepperApproximationGetter(v: Variable, scheduler: Scheduler) extends Stepper(null, null) {
  override def execute { scheduler.scheduleValues += v -> v.value.clone() }
}

/**
 * Scheduler. Only use in FactorGraph Factory.
 *
 * Here the Factor graph is calculated.
 * The calculation goes top down,
 * then (if more than 2 teams are present) the team differences are approximated
 * finaly everything goes back up and the results are given back
 *
 */
class Scheduler {
  // Steppers down:
  var stepDown1 = List[Stepper]()
  var stepDown2 = List[Stepper]()
  var stepDown3 = List[Stepper]()
  var stepDown4 = List[Stepper]() // Not used in case of more than 2 Teams

  // Steppers back up:
  var stepUp1 = List[Stepper]()
  var stepUp2 = List[Stepper]()
  var stepUp3 = List[Stepper]()
  var stepUp4 = List[Stepper]() // Not used in case of more than 2 Teams

  // Approximative Steppers:
  var stepApRight = List[Stepper]()
  var stepApLeft = List[Stepper]()

  // Used to caluculate the delta of each Approximation Run:
  var scheduleValues = HashMap[Variable, NormalDist]()
  var oldScheduleValues = HashMap[Variable, NormalDist]()

  /**
   * Schedules the Factor Graph, if only 2 Teams present. No Appriximation.
   */
  def scheduleTwoTeams {
    stepDown1.foreach(s => s.execute)
    stepDown2.foreach(s => s.execute)
    stepDown3.foreach(s => s.execute)
    stepDown4.foreach(s => s.execute)

    stepUp1.foreach(s => s.execute)
    stepUp2.foreach(s => s.execute)
    stepUp3.foreach(s => s.execute)
    stepUp4.foreach(s => s.execute)
  }

  /**
   * Schedules the FactorGraph, if more than 2 Teams are present. Approximative Approach used.
   */
  def scheduleApprox(implicit dflt: TrueSkillDefaultValues) {
    stepDown1.foreach(s => s.execute)
    stepDown2.foreach(s => s.execute)
    stepDown3.foreach(s => s.execute)

    // Approximative Aproach:
    stepApRight.foreach(s => s.execute);
    stepApLeft.foreach(s => s.execute);
    do {
      oldScheduleValues = scheduleValues.clone();
      stepApRight.foreach(s => s.execute);
      stepApLeft.foreach(s => s.execute);
    } while (!scheduleValues.keySet.forall(v => {
      dflt.withinDelta(scheduleValues(v).mean - oldScheduleValues(v).mean) &&
        dflt.withinDelta(scheduleValues(v).stDev - oldScheduleValues(v).stDev)
    }))

    stepUp2.foreach(s => s.execute)
    stepUp3.foreach(s => s.execute)
    stepUp4.foreach(s => s.execute)
  }
}