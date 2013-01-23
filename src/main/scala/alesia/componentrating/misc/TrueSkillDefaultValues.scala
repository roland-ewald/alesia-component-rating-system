package alesia.componentrating.misc

import scala.collection.mutable.HashMap

/**
 * Contains all default Parameter for the RankCalculator. 
 * Set your parameters in an instance of this class and pass it to FactorGraphFactory 
 * or your implementation of ComponentRatingSystem.
 * 
 * @author Jonathan Wienss
 *
 */
class TrueSkillDefaultValues( 
		    // Default Skill. Used for newly generated players
		    val defaultSkill:NormalDist = new NormalDist( 1.0/( (25.0/3.0)*(25.0/3.0) ), (1.0/( (25.0/3.0)*(25.0/3.0) )*25.0) ),
		    // Draw Propability
		    val pDraw: Double = 0.1, // see epsilon - Only use 0.0, 0.001, 0.01, 0.02, 0.03, 0.05, 0.1, ...., 0.4
		    // Beta
		    val beta: Double = (25.0/3.0) / 2.0,
		    // Tau. The value added to insecurity about player skills at each Team. See it as "Anti-Annealing", to keep Learning somwhat responsive.
		    val tau: Double = (25.0/3.0) / 100.0,     		    	
		    // Accuracy of the approximative Algorithm. Values around 0.0001 should be good. Raise this, if performance with to many Teams (>= 8 Teams) per Game becomes low.
		    val delta: Double = 0.0001,
		    // Debug
		    val debug : Boolean = false		    
		) { 
    
    // Draw Marign 
    // inverseCDF etc. from Wolfram Alpha, P_Draw = 0.1
    def epsilon(numberOfPlayers:Int) : Double = invCumErf(pDraw) * scala.math.sqrt( numberOfPlayers.toDouble ) * beta
       
    // True is Double is in Boundaries of delta
    def withinDelta(x:Double): Boolean = (x > -delta) && (x < delta) 

    override def toString = (""
		    +", defaultSkill: "+defaultSkill
		    +", DrawPropability: "+pDraw
		    +", beta: "+beta
		    +", tau: "+tau
		    +", delta: "+delta
		    )
		    
    // Source: calculated via Wolfram Alpha
    // Draw Propability -> inverse normal cumulative distribution function of ( Draw Propability / 2 + 0.5 )
    val invCumErf = HashMap[Double, Double](
            0.0  -> 0.0,
            0.05 -> 0.0627068,
            0.10 -> 0.125661,
            0.15 -> 0.189118,
            0.20 -> 0.253347,
            0.25 -> 0.318639,
            0.30 -> 0.38532,
            0.35 -> 0.453762,
            0.40 -> 0.524401,
            
            0.01 -> 0.0125335,
            0.02 -> 0.0250689,
            0.03 -> 0.0376083,
            
            0.001 -> 0.00125331
            ) 
}