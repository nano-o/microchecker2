package microchecker

import scala.collection.mutable
import scala.collection.mutable.HashSet
import collection.mutable.HashMap

class SimpleModelChecker[S, L] extends ModelChecker[S, L] 
{
  val Inf = Int.MaxValue
  var stateNum = 0;
  var statesMap = new HashMap[S, Int]()
  var transitMap = new HashMap[(Int, Int), L]()
  var initStateNum = 0;
  
  def addInitStates(sSet: Set[S], lts: LTS[S,L]) = 
  {
    var sset = sSet;
    while(!sset.isEmpty)
    {
      val lsopt = sset.find(_ => true)
      lsopt match {
        case Some(s) =>
          {
            if(!(lts.constraints map { c => c(s) } contains false))
            {
              statesMap += (s -> stateNum);
              stateNum += 1;
              initStateNum += 1;
            }
            sset -= s
          }
        case None => throw new RuntimeException("There are non element in the initial state set")
      }
    }
  }
  
  def addNewStates(sID: Int, state: S, lsSet: Set[(L,S)], lts: LTS[S,L]) : Set[S] = 
  {
    var sSet = Set[S]()
    var lsS = lsSet;
    while(!lsS.isEmpty)
    {
      val lsopt = lsS.find(_ => true)
      lsopt match {
        case Some((l,s)) =>
          {
            if(!statesMap.contains(s) & !(lts.constraints map { c => c(s) } contains false) )
            {
              statesMap += (s -> stateNum);
              transitMap += ((sID, stateNum) -> l)
              stateNum += 1;
              sSet += s;
            }
            val ls = (l,s)
            lsS -= ls
          }
        case None => throw new RuntimeException("There are non element in the label-state set")
      }
    }
    sSet
  }
  
  def check(lts: LTS[S,L]) : Unit = 
  {
    val unexplored : mutable.Set[S] = mutable.Set();
    val initStates = lts.initialStates
    unexplored ++= initStates
    
    addInitStates(initStates, lts)
    
    while (!unexplored.isEmpty) 
    {
      val sopt = unexplored.find(_ => true)
      sopt match 
      {
        case None => throw new RuntimeException("could not find any element in a non-empty set, something's weird...")
        case Some(s) => 
          {
            val sID = statesMap.getOrElse(s, 0)
            
            lts.invariants foreach {i =>
              if (!i(s)) {
                printTrace(sID)
                throw new RuntimeException("Invariant violated: " + s)
              }
            }
            
            var sucls = lts.successors(s)
            var sucs = addNewStates(sID, s, sucls, lts)
            unexplored ++= sucs
            unexplored -= s
          }
      }
    }
  }
  
  //print out the path from the initial state to the error state
  def printTrace(n: Int) = 
  {
    var path = new Array[Int](n + 1)
    var dist = new Array[Int](n + 1)
    var visited = new Array[Boolean](n + 1)
    
    for (i <- 0 to n) 
    {
      if(transitMap.contains(0, i) && i != 0)
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
        if(!visited(k) && transitMap.contains((u, k)) && min + 1 < dist(k))
        {
          dist(k) = min + 1;
          path(k) = u;
        }
      }
    }

    var from = 0
    var mid = 0
    println("=================================================================================================================================")
    println("Error in state: " + n)
    
    val stack = new scala.collection.mutable.Stack[Int] 
    var to = n;
    while(to > initStateNum)
    {
      stack.push(to);
      to = path(to);
    }
    stack.push(to);
    while(!stack.isEmpty)
    {
      from = stack.top;
      println("--------------------------------------------------------------------------------------------------------------------------------------------")
      printState(from, indexMap(from));
      stack.pop();
      if(!stack.isEmpty)
      {
        mid = stack.top;
        println("  ->->->->->->->->->->->->->->->-> lable:" + transitMap((from, mid)) + " ->->->->->->->->->->->->->->->->");
      }
    }
  }
  
  def printState(id: Int, node: S) = 
  {
    println("state: " + id)
    println(node)
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