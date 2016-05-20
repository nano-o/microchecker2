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
  
  def printDiff(index1: Int, node1: MultiPaxos4.mp_state_ext[Int, Unit], index2:Int, node2: MultiPaxos4.mp_state_ext[Int, Unit]) = 
  {
    println("----------------------------------------------------------State Difference-------------------------------------------------------------") 
    println("previous state: " + index1 + "  ->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->   succeeding state:" + index2);
    node1 match {
        case MultiPaxos4.mp_state_exta(node_states, network, more) =>
          {
            node2 match {
              case MultiPaxos4.mp_state_exta(node_states2, network2, more2) =>
                {
                  for(i <- 0 to maxAccID)
                        {
                          val astate_ext1 = finfun_apply(node_states, accs(i))
                          val astate_ext2 = finfun_apply(node_states2, accs(i))
                          printAccStateDiff(accs(i), astate_ext1, astate_ext2)
                        }
                      }
                    println("--------------------------------------------------------------------------------------------------------------------------------------------")
                    if(network == network2)
                      println("  network_packets: " + network);
                    else
                    {
                      println("  network_packets1: " + network);
                      println("  network_packets2: " + network2);
                    }
                }
          }
    }
  }

  def printAccStateDiff(acc: Nat, astate_ext1: MultiPaxos4.acc_state_ext[Int, Unit], astate_ext2: MultiPaxos4.acc_state_ext[Int, Unit])
  {
    astate_ext1 match {
      case MultiPaxos4.acc_state_exta(accID, leader, acceptors, ballot, decided, vote, last_ballot, onebs,
        twobs, next_inst, last_decision, working_instances, commit_buffer,
          last_commited, snapshot_reference, snapshot_proposal, more) =>
      {
        astate_ext2 match {
          case MultiPaxos4.acc_state_exta(accID2, leader2, acceptors2, ballot2, decided2, vote2, last_ballot2, onebs2,
            twobs2, next_inst2, last_decision2, working_instances2, commit_buffer2,
              last_commited2, snapshot_reference2, snapshot_proposal2, more2) =>
                {
                  println("accID: " + accID);
                  if(!(leader == leader2)) println("        leader1: " + leader + "        leader2: " + leader2)
                  if(!(acceptors == acceptors2)) println("        acceptors1: " + acceptors + "        acceptors2: " + acceptors2)
                  if(!(ballot == ballot2)) println("        ballot1: " + ballot + "        ballot2: " + ballot2)
                  if(!(decided == decided2)) println("        decided1: " + decided + "        decided2: " + decided2)
                  if(!(vote == vote2)) println("        vote1: " + vote + "        vote2: " + vote2)
                  if(!(last_ballot == last_ballot2)) println("        last_ballot1: " + last_ballot + "        last_ballot2: " + last_ballot2)
                  if(!(onebs == onebs2)) println("        onebs1: " + onebs + "        onebs2: " + onebs2)
                  if(!(twobs == twobs2)) println("        twobs1: " + twobs + "        twobs2: " + twobs2)
                  if(!(next_inst == next_inst2)) println("        next_inst1: " + next_inst + "        next_inst2: " + next_inst2)
                  if(!(last_decision == last_decision2)) println("        last_decision1: " + last_decision + "        last_decision2: " + last_decision2)
                  if(!(working_instances == working_instances2)) println("        working_instances1: " + working_instances + "        working_instances2: " + working_instances2)
                  if(!(commit_buffer == commit_buffer2)) println("        commit_buffer1: " + commit_buffer + "        commit_buffer2: " + commit_buffer2)
                  if(!(last_commited == last_commited2)) println("        last_commited1: " + last_commited + "        last_commited2: " + last_commited2)
                }
        }
      }
    }
  }
}