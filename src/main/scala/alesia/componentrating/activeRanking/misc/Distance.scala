package alesia.componentrating.activeRanking.misc

import scala.collection.mutable.HashMap

/**
 * Distance: Function of two arrays (Lists) of "Things"
 * 		= 0 if both are the same
 * 		> 0 otherwise
 * 
 * @author Jonathan Wienss
 */
abstract class Distance {
    /**
	 * The arrays/lists are provided in HashMaps (Element) -> (Position)
	 * This allows empty positions etc.
     */
    def getDistance[T](first:HashMap[T,Int], second:HashMap[T,Int]) : Int
    
    /**
     * Creates the intersection of Keysets of hashmaps
     */
	def intersection[T](map: HashMap[T,Int], intersectWith:HashMap[T,Int]) : (HashMap[T,Int]) = {
	    val result = HashMap[T,Int]()
	    map.keySet.foreach(e=>if(intersectWith.keySet.contains(e)) result+=e->map(e))
	    return result 
	}    	
    
    def getDistance[T](first:List[T], second:List[T]):Int = {
        return getDistance(listToHM(first),listToHM(second))
    }
    
    def getDistanceIntersected[T](first:HashMap[T,Int], second:HashMap[T,Int]) : Int = {
    	return getDistance(intersection(first,second),intersection(second,first))
    }
    def getDistanceIntersected[T](first:List[T], second:List[T]) : Int = {
    	val firstMap = HashMap[T,Int]()
        val secondMap = HashMap[T,Int]()        
        
        var i = 0
        first.foreach(e=>{firstMap+=e->i;i=i+1})
        i=0
        second.foreach(e=>{secondMap+=e->i;i=i+1})
        
    	return getDistance(intersection(listToHM(first),listToHM(second)),intersection(listToHM(second),listToHM(first)))
    }
    
    def listToHM[T](list:List[T]):HashMap[T,Int]={
        val hM = HashMap[T,Int]()
        
        var i = 0
        list.foreach(e=>{hM+=e->i;i=i+1})
        hM
    }
}

/**
 * Hamming Distance ("Number of wrong digits")
 * 
 * Reimplemented to be invariant vs missing ranks (and thus intersections from above which very likely have missing ranks)
 * @author Jonathan Wienss
 * 
 * missing rank: (a -> 1), (b -> 2), (c -> 4)  [<- should be rank 3]
 *
 */
class HammingDistance extends Distance {
    def getDistance[T](first:HashMap[T,Int], second:HashMap[T,Int]) : Int = {
        var tFirst = first.clone
        var tSecond = second.clone
        var dist = 0
        
        first.keySet.foreach(key=>{
            if(getMin(tFirst) != getMin(tSecond)) dist = dist + 1
            tFirst.remove(getMin(tFirst))
            tSecond.remove(getMin(tSecond))
        })
        

//        first.keySet.foreach(key => if(first(key) != second(key)) dist = dist + 1)
        return dist
    }
    
    def getMin[T](map:HashMap[T,Int]):T = {
        var candidate = map.keySet.head
        map.keySet.foreach(key=>if(map(key)<map(candidate)) candidate = key)
        candidate
    }
    
    override def toString = "Haming Distance"
}

/**
 * Number of Inversions as distance
 * One Inversion: Two Places are interchanged
 * Number of Inversions: Min steps of interchanging of two places to reach correct ordering
 * 
 * @author Jonathan Wienss
 */
class NumberOfInversionsDistance extends Distance { 
    def getDistance[T](f:HashMap[T,Int], s:HashMap[T,Int]):Int = {
        val f2 = f.clone()   
        var fSorted = f2.keySet.toList.sortBy(f2)
        val s2 = s.clone()
        var sSorted = s2.keySet.toList.sortBy(s2)
        
        var result = 0
        while(!(fSorted==List() && sSorted==List())) { {
              if( fSorted.head != sSorted.head) {
                  //inverte
                  val a = depth(fSorted.head, sSorted)
                  result = result + a._1
                  sSorted = a._3.head :: a._2 ::: a._3.tail
              }
              fSorted = fSorted.tail
              sSorted = sSorted.tail
          }
        }
        return result
    }
    private def depth[T](x:T, l:List[T]):(Int, List[T], List[T]) = { 
            if(l.head==x) Tuple3(0, List(), l) 
            else {
                var r=depth(x,l.tail)
                Tuple3(r._1+1,l.head::r._2, r._3) 
                }
            } 
        override def toString = "Number Of Inversions Distance III"
}