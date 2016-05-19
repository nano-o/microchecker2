package microchecker

import MicroCheckerLib._
import MultiPaxos4._

class PaxosLTS extends LTS[mp_state_ext[Int, Unit], mp_action[Int]] {
  
  val cmd = 2
  val maxInst = 1
  val maxAccID = 2
  val bal_bound = new Nat(1)
  val inst_bound = new Nat(maxInst + 1)
  
  var actionBuf = collection.mutable.ListBuffer.empty[mp_action[Int]]
  
  var accs = new Array[Nat](maxAccID + 1)
  for(i <- 0 to maxAccID)
    accs(i) = new Nat(i)
  
  def initFreeLabel = {
    for(a <- 0 to maxAccID)
    {
      val acc = new Nat(a)
      actionBuf += MultiPaxos4.Send_1as(acc)
      for(v <- 1 to cmd)
      {
        actionBuf += MultiPaxos4.Propose(acc, MultiPaxos4.Cmd(v))
        for(i <- 1 to maxInst)
        {
          val inst = new Nat(i)
          actionBuf += MultiPaxos4.Learn(acc, inst, MultiPaxos4.Cmd(v))
        }
      }
    }
  }
  
  override def initialStates = Set(mp_start[Int]);
  
  override def successors(s: mp_state_ext[Int, Unit]) : Set[(mp_action[Int],mp_state_ext[Int, Unit])] = 
  {
    initFreeLabel;
    
    var curAction = actionBuf.head
    var newStates : Set[(mp_action[Int], mp_state_ext[Int, Unit])] = Set();
    
    actionBuf.foreach { it => 
        {
          try
          {
            val newState = mp_transit(s, it)
            newStates += (Tuple2(it, newState))
          }
          catch
          {
            case e: Exception => {println("Error at " + s + ", msg: " + it); System.exit(1)}
            //printTrace(it, curStateID); 
          }
        }
          
      }
      s match {
        case MultiPaxos4.mp_state_exta(node_states, network, more) =>
          {
            var packetSet = fset(network)
            packetSet match {
              case seta(packetList) =>
                {
                  packetList.foreach { it => {
                    it match {
                      case MultiPaxos4.Packet(src, dest, msg) =>
                        {
                          msg match {
                            case MultiPaxos4.Phase1a(acp, bal) =>
                              {
                                if(acp == src)
                                {
                                  try
                                  {
                                    curAction = MultiPaxos4.Receive_1a_send_1b(src, dest, bal)
                                    val newState = MultiPaxos4.mp_transit(s, curAction)
                                    newStates += (Tuple2(curAction, newState))
                                  }
                                  catch
                                  {
                                    case e: Exception => {println("Error at " + s + ", msg: " + curAction); System.exit(1)}
                                    //printTrace(curAction, curStateID); 
                                  }
                                }
                              }
                            case MultiPaxos4.Phase1b(vs, bal, acp) =>
                              {
                                if(acp == src)
                                {
                                  try
                                  {
                                    curAction = MultiPaxos4.Receive_1b(src, dest, vs, bal)
                                    val newState = MultiPaxos4.mp_transit(s, curAction)
                                    newStates += (Tuple2(curAction, newState))
                                  }
                                  catch
                                  {
                                    case e: Exception => {println("Error at " + s + ", msg: " + curAction); System.exit(1)}
                                  }
                                }
                              }
                            case MultiPaxos4.Phase2a(inst, bal, cmdv, leader) =>
                              {
                                if(src == leader)
                                {
                                  try
                                  {
                                    curAction = MultiPaxos4.Receive_2a_send_2b(src, dest, inst, bal, cmdv)
                                    val newState = MultiPaxos4.mp_transit(s, curAction)
                                    newStates += (Tuple2(curAction, newState))
                                  }
                                  catch
                                  {
                                    case e: Exception => {println("Error at " + s + ", msg: " + curAction); System.exit(1)}
                                  }
                                }
                              }
                            case MultiPaxos4.Phase2b(inst, bal, acp, cmdv) =>
                              {
                                if(src == acp)
                                {
                                  try
                                  {
                                    curAction = MultiPaxos4.Receive_2b(src, dest, inst, bal, cmdv)
                                    val newState = MultiPaxos4.mp_transit(s, curAction)
                                    newStates += (Tuple2(curAction, newState))
                                  }
                                  catch
                                  {
                                    case e: Exception => {println("Error at " + s + ", msg: " + curAction); System.exit(1)}
                                  }
                                }
                              }
                            case MultiPaxos4.Fwd(v) =>
                              {
                                try
                                {
                                  curAction = MultiPaxos4.Receive_fwd(src, dest, v)
                                  val newState = MultiPaxos4.mp_transit(s, curAction)
                                  newStates += (Tuple2(curAction, newState))
                                }
                                catch
                                {
                                  case e: Exception => {println("Error at " + s + ", msg: " + curAction); System.exit(1)}
                                }
                              }
                          }
                        }
                      }
                    }
                  
                  }
                }
              case _ => {}
            }
          }
      }
    
    newStates
  }
  
  def checkSafeAt(s : mp_state_ext[Int, Unit]) : Boolean = {
    for(iiter  <- 1 to maxInst) 
    {
      val iinst = new Nat(iiter);
      if (!safe_at(s, iinst)) 
        return false
    }
    true
  }
  override def invariants = Set(s => checkSafeAt(s));
  
  override def constraints = Set(s => ballot_constraint(s, bal_bound), s => inst_constraint(s, inst_bound));
}