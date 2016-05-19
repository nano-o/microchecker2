package microchecker

import scala.collection.mutable
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import collection.mutable.HashMap
import scala.util.control._

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
  case class Error(s : Int) extends ExitStatus
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
          statesMap.get(n) match { case Some(n_) => return Error(n_) }
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
  
  override def check : Option[(S,List[(L,S)])] = 
  {
    // add initial states to unexplored queue
    val initStates = lts.initialStates filter filterStateP
    unexplored ++= initStates
    

    // assign an identifier to each initial state
    initStates foreach { case s => {
    	if (!checkInvariants(s)) {
    		logger.log(printState(stateNum, s))
    		return Some(s,List())
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
          return None
        }
        case Error(n) => {
          logger.log("The number of total states = " + statesMap.size)
          
          return Some(bfs(n))
        } 
      }
    }
    None
  }
  
  def bfs(n: Int) : (S,List[(L,S)]) = 
  {
    var path = new Array[Int](n + 1)
    
    var queue = new Queue[Int];
    var visited = new Array[Boolean](n + 1)
    for (i <- 0 to n) 
    {
      path(i) = -1;
      visited(i) = false;
    }
    
    queue.enqueue(n)
    visited(n) = true;
    
    var from = 0
    var mid = 0;
    var to = 0;
    
    val loop = new Breaks;
    loop.breakable 
    {
      while (!queue.isEmpty)
  	  {
        mid = queue.dequeue()
        var stransit = transitMap.getOrElse(mid, Set())
        stransit foreach {
          case (label, sID) =>
            {
              if (sID < initStateNum)
              {
                path(sID) = mid
                from = sID
                loop.break;
              }
              if(!visited(sID))
              {
                path(sID) = mid
                queue.enqueue(sID)
                visited(sID) = true;
              }
            }
        }
      }
    }
    
    // get the trace as a list.
    var result = List[(L,S)]()
    
    mid = from;
    while(to != n)
    {
      to = path(mid);
      result = result ::: List((transitLabel(mid, to), indexMap(to)))
      mid = to;
    }
    
    (indexMap(from),result)
  }
  
  def transitContains(i: Int, j: Int) : Boolean = 
  {
    val stransit = transitMap.getOrElse(j, Set())
    stransit foreach {
      case (lable, index) =>
        {
          if(i == index)
            return true
        }
    }
    false
  }
  
  def transitLabel(i: Int, j: Int) : L = 
  {
    val stransit = transitMap.getOrElse(j, Set())
    stransit foreach {
      case (lable, index) =>
        {
          if(i == index)
            return lable
        }
    }
    throw new RuntimeException("should have found the label")
  }
  
  // Apply Dijkstra's single-source shortest-path algorithm to find the shortest path from the error state to an initial state.
  // n is the index of the error state.
  // we return an array of indices forming a linked list whose head is at position n (the array starts at 0).
  // this linked list is the error trace (in reverse order)
  // see: https://en.wikipedia.org/wiki/Dijkstra's_algorithm
  def dijkstra(n : Int) : (S,List[(L,S)]) = {
    var path = new Array[Int](n + 1) // path
    var dist = new Array[Int](n + 1) // distance
    var visited = new Array[Boolean](n + 1)
    
    for (i <- 0 to n) 
    {
      if(transitContains(0, i) && i != 0)
      {
        dist(i) = 1
        path(i) = 0
      }
      else
      {
        dist(i) = Inf
        path(i) = -1
      }
      visited(i) = false;
    }
    path(0) = 0;
    dist(0) = 0;
    visited(0) = true;
    for(i <- 1 to n)
    {
      var min = Inf;
      var u = 0;
      for(j <- 0 to n)
      {
        if(!visited(j) && dist(j) < min)
        {
          min = dist(j);
          u = j;
        }
      }
      visited(u) = true;
      for(k <- 0 to n)
      {
        if(!visited(k) && transitContains(u, k) && min + 1 < dist(k))
        {
          dist(k) = min + 1;
          path(k) = u;
        }
      }
    }
    
    // get the trace as a list.
    var result = List[(L,S)]()
    var from = 0
    var mid = 0
    var to = n;
    // we use a stack to reverse the linked-list representing the trace.
    val stack = new scala.collection.mutable.Stack[Int] 
    while(to != 0)
    {
      stack.push(to);
      to = path(to);
    }
    stack.push(to);
    while(!stack.isEmpty)
    {
      from = stack.top;
      stack.pop();
      if(!stack.isEmpty)
      {
        mid = stack.top;
        result = result ::: List((transitLabel(from, mid),indexMap(mid)))
      }
    }
    
    (indexMap(to),result)
  }
  
  override def printTrace( trace : (S,List[(L,S)]) ) = 
  {
    println("=================================================================================================================================")
    println("Error in state: " + statesMap.getOrElse(trace._2.last._2, 0))
    
    logger.log("------------------------------------------------------------------------------------------------------------------------------")
    logger.log("state: " + statesMap.getOrElse(trace._1, 0).toString())
    logger.log(trace._1.toString)
    
    trace._2 foreach { case (l,s) => {
      logger.log("->->->->->->->->->->->->->->->-> msg:" + l + " ->->->->->->->->->->->->->->->->");
      logger.log("state: " + statesMap.getOrElse(s, 0).toString())
      logger.log(s.toString)
    } }
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