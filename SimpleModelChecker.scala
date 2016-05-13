package microchecker

import scala.collection.mutable
import scala.collection.mutable.HashSet
import collection.mutable.HashMap

class SimpleModelChecker[S, L](lts_ : LTS[S,L], l : Logger) extends ModelChecker[S, L] 
{
  val logger : Logger = l
  val Inf = Int.MaxValue
  var stateNum = 0; // used to give an index to new states
  var statesMap = new HashMap[S, Int]() // state to index
  var transitMap = new HashMap[Int, mutable.Set[(L,Int)]]() // the _inverse_ transition relation uncovered so far
  var initStateNum = 0;
  val unexplored : mutable.Set[S] = mutable.Set()
  val lts : LTS[S,L] = lts_
  
  def filterStateP(s : S) = 
      !(lts.constraints map { c => c(s) } contains false) & !statesMap.contains(s)
  
  abstract class ExitStatus
  case object Error extends ExitStatus
  case object Ok extends ExitStatus
  case object Finished extends ExitStatus
      
  def dequeueState : ExitStatus = {
    val s = unexplored.find(_ => true) match { 
      case Some(s_) => s_
      case None => return Finished 
    }
    val sId = statesMap.get(s) match {
      case Some(id) => id
      case None => throw new RuntimeException("should not happen")
    }
    // set of successor transitions:
    val trs = lts.successors(s) filter { case (l,s) => filterStateP(s) }
    
    // add successor states to the queue of unepxlored states
    unexplored ++= trs map { case (l,s) => s }
    
    // give an id to each successor state and add the corresponding transition to the transition map
    trs foreach { 
      case (l, n) => {
        if (!statesMap.contains(n)) 
        {
          statesMap += (n -> stateNum)
          logger.debug(printState(stateNum, n))
          transitMap += (stateNum -> mutable.Set((l, sId)));
          stateNum += 1
        }
        else {
          statesMap.get(n) match {
            case Some(x) => 
                transitMap.get(x) match {
                  case Some(stransit) => stransit += new Tuple2(l, sId)
                  case None => transitMap += (x -> mutable.Set((l, sId)))
                }
            case None => throw new RuntimeException("should not happen")
          }
        }
        if (!checkInvariants(n)) {
          statesMap.get(n) match { case Some(n_) => printTrace(n_) }
          return Error
        }
      }
    }
    
    // remove s from the queue of unexplored states
    unexplored -= s
    
    Ok
  }
  
  def checkInvariants(s : S) : Boolean = {
    lts.invariants foreach { inv => if (!inv(s)) return false }
    true
  }
  
  def check : Boolean = 
  {
    // add initial states to unexplored queue
    val initStates = lts.initialStates filter filterStateP
    unexplored ++= initStates
    

    // assign an identifier to each initial state
    initStates foreach { case s => {
    	if (!checkInvariants(s)) {
    		logger.log(printState(stateNum, s))
    		return false
    	}
    	statesMap += (s -> stateNum)
    			logger.debug("Initial state added:" + printState(stateNum, s))
    			stateNum += 1
    } }
    
    while (true) {
      dequeueState match {
        case Ok => ()
        case Finished => {
          logger.log("The number of total states = " + statesMap.size)
          return true
        }
        case Error =>{
          logger.log("The number of total states = " + statesMap.size)
          return false
        } 
      }
    }
    
    true
  }
  
  def printTrace(n: Int) : Unit = 
  {
    if(transitMap.contains(n))
    {
      logger.log(printState(n, indexMap(n)));
      var transitSet = transitMap.getOrElse(n, Set())
      transitSet foreach { case (l, index) =>
        {
          logger.log("  ->->->->->->->->->->->->->->->-> lable:" + l + " ->->->->->->->->->->->->->->->->");
          printTrace(index);
        }
      }
    }
    else
    {
      logger.log(printState(n, indexMap(n)))
    }
  }
  
  def printState(id: Int, node: S) : String = 
  {
    "state: " + id + "\n" +
    node.toString
  }
  
    //Search for the node with its index
  def indexMap(index: Int) : S = 
  {
    var indexNode = 0
    statesMap.foreach {it => 
        {
          if(index == it._2)
            return it._1;
        }
    }
    return statesMap.head._1
  }
}