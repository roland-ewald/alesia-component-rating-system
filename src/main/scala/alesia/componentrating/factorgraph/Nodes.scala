package alesia.componentrating.factorgraph

import scala.collection.mutable.HashMap

import alesia.componentrating.misc.NormalDist
import alesia.componentrating.misc.Player
import alesia.componentrating.misc.TrueSkillDefaultValues

/**
 * The nodes of a factor graph.
 * 
 * @author Jonathan Wienss
 */

abstract class Factor( name: String ) {   
    var neighborsUp = List[Variable]()
    var neighborDown: Variable = null  

    def update(v:Variable)
    
    override def toString: String = name
}

/**
 * A Variable 
 */
class Variable( val name: String, var value: NormalDist )(implicit dflt:TrueSkillDefaultValues) { 
    private val lastMessage = new HashMap[Factor,NormalDist]() // Holds the last message for each neighbor
    
    // Multiply the update into this variable
    def updateProduct(valueNew:NormalDist, from:Factor) {        
        value = (value / getLastMessage(from) ) * valueNew 
        lastMessage += from -> valueNew.clone()
        if(dflt.debug) System.out.println( toString+": "+value.toString+" from "+ from )
    }
    
    // The update is just set in this variable
    def updateSet(valueNew:NormalDist, from:Factor) {        
        lastMessage += from -> getLastMessage(from) * valueNew / value
        value = valueNew.clone()
        if(dflt.debug) System.out.println( toString+": "+value.toString+" from "+ from )
    }
     
    // this encapsulates lastMessage
    def getLastMessage(f:Factor):NormalDist = {
         if(lastMessage.contains(f)) return lastMessage(f)
         else return new NormalDist(0, 0)
     }
     
     override def toString: String = name 
}

class FactorPrior( name: String, player: Player )(implicit dflt:TrueSkillDefaultValues) extends Factor( name ) {
    override def update(v:Variable) {
    	val message: NormalDist = new NormalDist( v.value.precision + 1 / ( player.skill.variance + dflt.tau * dflt.tau ),
        v.value.precAdjustMean + player.skill.mean / ( player.skill.variance + dflt.tau * dflt.tau ) ) 
        
        v.updateSet( message, FactorPrior.this )
    }
}

class FactorPerformance( name: String, player: Player )(implicit dflt:TrueSkillDefaultValues) extends Factor( name ) {
    override def update(v:Variable) {
    	val otherVariable: Variable = if( v==neighborsUp.head ) neighborDown else if( v== neighborDown ) neighborsUp.head else null

    	def a = 1 / ( 1 + ( dflt.beta * dflt.beta * ( otherVariable.value.precision - otherVariable.getLastMessage(FactorPerformance.this).precision) ) )
    	val message: NormalDist = new NormalDist ( a * ( otherVariable.value.precision - otherVariable.getLastMessage(FactorPerformance.this).precision ),    
    			a * ( otherVariable.value.precAdjustMean - otherVariable.getLastMessage(FactorPerformance.this).precAdjustMean) )
	
    	v.updateProduct( message, FactorPerformance.this )
    }
}

class FactorSum( name: String )(implicit dflt:TrueSkillDefaultValues) extends Factor( name ) {
    val prefactors = new HashMap[Variable, Double]() 
    
    override def update(v:Variable) {
        val neighbors = neighborDown :: neighborsUp 
        
        var temp: Double = 0.0
        neighbors.foreach( n => ( if ( n != v ) temp = temp + ( a( n,v ) * a( n,v ) / ( n.value.precision - n.getLastMessage(FactorSum.this).precision ) ) ) )
        val precision = 1 / temp 
                
        temp = 0.0
        neighbors.foreach( n => {
        	( if ( n != v ) temp = (temp + ( a( n,v ) * ( n.value.precAdjustMean - n.getLastMessage(FactorSum.this).precAdjustMean ) / 
        	        ( n.value.precision - n.getLastMessage(FactorSum.this).precision) ) ) )  
        }  )
        val precisionAdjustedMean = precision * temp

        v.updateProduct(new NormalDist( precision, precisionAdjustedMean ), FactorSum.this )
    }
    
    def a(n:Variable, v:Variable):Double = -prefactors(n) / prefactors(v)
}

class FactorGaussianComparisonWin( name: String, numberOfPlayers:Int )(implicit dflt:TrueSkillDefaultValues) extends Factor( name ) {
    def c( x: Variable ):Double = x.value.precision - x.getLastMessage(FactorGaussianComparisonWin.this).precision 
    def d( x: Variable ):Double = x.value.precAdjustMean - x.getLastMessage(FactorGaussianComparisonWin.this).precAdjustMean

    def vF( t: Double, eps: Double ):Double = ( new NormalDist( 1, 0 ).density( t - eps ) ) / ( new NormalDist( 1, 0 ).cumulativePropability( t - eps ) )
    def wF( t: Double, eps: Double ):Double = vF( t, eps ) * ( vF( t, eps ) + t - eps )
    
    def divisor(x:Variable): Double = 1.0 - wF( d( x ) / scala.math.sqrt( c( x ) ), dflt.epsilon(numberOfPlayers) * scala.math.sqrt( c( x ) ) )
    def dividend(x:Variable):Double = d( x ) + scala.math.sqrt( c( x ) ) * vF( d( x ) / scala.math.sqrt( c( x ) ), dflt.epsilon(numberOfPlayers) * scala.math.sqrt( c( x ) ) ) 
    
    override def update(v:Variable) {
    	val x = v // because has only one neighbor

        val message = new NormalDist( c( x ) / divisor(x),
            dividend(x) / divisor(x) )

	    v.updateSet( message, FactorGaussianComparisonWin.this ) 
    }
}