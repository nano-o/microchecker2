package microchecker

import Array._
import collection.JavaConversions._
import collection.mutable.HashMap
import collection.mutable.HashSet
import collection.mutable.ListBuffer
import scala.math
import MicroCheckerLib._
import MultiPaxos4._

object TestHashCode {
  def main(args: Array[String]) 
  {
//    val maxNum = 100;
//    var statesMap = new HashMap[Nat, Int]() 
//    var stateNum = 0
//    
//    val maxLoop = 20000;
//    
//    for ( i <- 0 until maxNum)
//    {
//      val testCase = new Nat(i)
//      statesMap += (testCase -> stateNum);
//      stateNum += 1;
//    }
//    println("The size of the constructed hashMap is " + statesMap.size)
//    
//    val r = new scala.util.Random
//    for(i <- 0 to maxLoop)
//    {
//      val testCase = new Nat(r.nextInt(maxNum))
//      if(!statesMap.contains(testCase))
//      {
//        statesMap += (testCase -> stateNum);
//        stateNum += 1;
//      }
//    }
//    println("The size of the random-tested hashMap is " + statesMap.size)
    
//    val maxNum = 100;
//    var statesMap = new HashMap[cmd[Int], Int]() 
//    var stateNum = 0
//    
//    val maxLoop = 20000;
//    
//    for ( i <- 0 until maxNum)
//    {
//      if (i == 0)
//      {
//        val testCase = new NoOp[Int]()
//        statesMap += (testCase -> stateNum);
//        stateNum += 1;
//      }
//      else
//      {
//        val testCase = new Cmd[Int](i)
//        statesMap += (testCase -> stateNum);
//        stateNum += 1;
//      }
//    }
//    println("The size of the constructed hashMap is " + statesMap.size)
//   
//    val r = new scala.util.Random
//    for(i <- 0 to maxLoop)
//    {
//      val test = r.nextInt(maxNum);
//      if (test == 0)
//      {
//        val testCase = new NoOp[Int]()
//        if(!statesMap.contains(testCase))
//        {
//          statesMap += (testCase -> stateNum);
//          stateNum += 1;
//        }
//      }
//      else
//      {
//        val testCase = new Cmd[Int](test)
//        if(!statesMap.contains(testCase))
//        {
//          statesMap += (testCase -> stateNum);
//          stateNum += 1;
//        }
//      }
//    }
//    println("The size of the random-tested hashMap is " + statesMap.size)
    
//    val maxAcc = 5;
//    val maxBallot = 4;
//    val maxInst = 5;
//    val maxCmd = 5;
//    var statesMap = new HashMap[msg[Int], Int]() 
//    var stateNum = 0
//    
//    val maxLoop = 100000;
//    
//    for (acc <- 0 until maxAcc)
//    {
//      val accNat = new Nat(acc);
//      for (ballot <- 0 until maxBallot)
//      {
//        val ballotNat = new Nat(ballot);
//        val testcase1a = new Phase1a[Int](accNat, ballotNat)
//        statesMap += (testcase1a -> stateNum);
//        stateNum += 1;
//        
//        for (inst <- 0 until maxInst)
//        {
//          val instNat = new Nat(inst);
//          for (cmd <- 0 until maxCmd)
//          {
//            if(cmd == 0)
//            {
//              val testcmd = new NoOp[Int]()
//              val testcase2a = new Phase2a[Int](instNat, ballotNat, testcmd, accNat);
//              if(!statesMap.contains(testcase2a))
//              {
//                statesMap += (testcase2a -> stateNum);
//                stateNum += 1;
//              }
//              val testcase2b = new Phase2b[Int](instNat, ballotNat, accNat, testcmd);
//              if(!statesMap.contains(testcase2b))
//              {
//                statesMap += (testcase2b -> stateNum);
//                stateNum += 1;
//              }
//              val testvote = new Vote[Int](instNat, testcmd)
//              if(!statesMap.contains(testvote))
//              {
//                statesMap += (testvote -> stateNum);
//                stateNum += 1;
//              }
//            }
//            else
//            {
//              val testcmd = new Cmd[Int](cmd)
//              val testcase2a = new Phase2a[Int](instNat, ballotNat, testcmd, accNat);
//              if(!statesMap.contains(testcase2a))
//              {
//                statesMap += (testcase2a -> stateNum);
//                stateNum += 1;
//              }
//              val testcase2b = new Phase2b[Int](instNat, ballotNat, accNat, testcmd);
//              if(!statesMap.contains(testcase2b))
//              {
//                statesMap += (testcase2b -> stateNum);
//                stateNum += 1;
//              }
//              val testvote = new Vote[Int](instNat, testcmd)
//              if(!statesMap.contains(testvote))
//              {
//                statesMap += (testvote -> stateNum);
//                stateNum += 1;
//              }
//            }
////            val default_finfun:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_const[Option[(cmd[Int], Nat)], Nat](None)
////            val lastvote:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_update_code[Nat, Option[(cmd[Int], Nat)]](default_finfun, instNat, Some(new Cmd[Int](cmd), ballotNat))
////            val testcase1b = new Phase1b(lastvote, ballotNat, accNat)
////            if(!statesMap.contains(testcase1b))
////            {
////              statesMap += (testcase1b -> stateNum);
////              stateNum += 1;
////            }
//            val testcasefwd = new Fwd[Int](cmd)
//            if(!statesMap.contains(testcasefwd))
//            {
//              statesMap += (testcasefwd -> stateNum);
//              stateNum += 1;
//            }
//          }
//        }
//      }
//    }
//    println("The size of the constructed hashMap is " + statesMap.size)
//   
//    val accrandom = new scala.util.Random
//    val ballotrandom = new scala.util.Random
//    val cmdrandom = new scala.util.Random
//    val instrandom = new scala.util.Random
//    for(i <- 0 to maxLoop)
//    {
//      val testacc = new Nat(accrandom.nextInt(maxAcc));
//      val testballot = new Nat(ballotrandom.nextInt(maxBallot));
//      val testcmd = cmdrandom.nextInt(maxCmd);
//      val testinst = new Nat(instrandom.nextInt(maxInst));
//    
//      val testcase1a = new Phase1a[Int](testacc, testballot)
//      
//      if(!statesMap.contains(testcase1a))
//      {
//        statesMap += (testcase1a -> stateNum);
//        stateNum += 1;
//      }
//      
//      if (testcmd == 0)
//      {
//        val testcasecmd = new NoOp[Int]()
//        val testcase2a = new Phase2a[Int](testinst, testballot, testcasecmd, testacc);
//        if(!statesMap.contains(testcase2a))
//        {
//          statesMap += (testcase2a -> stateNum);
//          stateNum += 1;
//        }
//        val testcase2b = new Phase2b[Int](testinst, testballot, testacc, testcasecmd);
//        if(!statesMap.contains(testcase2b))
//        {
//          statesMap += (testcase2b -> stateNum);
//          stateNum += 1;
//        }
//        val testvote = new Vote[Int](testinst, testcasecmd)
//        if(!statesMap.contains(testvote))
//        {
//          statesMap += (testvote -> stateNum);
//          stateNum += 1;
//        }
//      }
//      else
//      {
//        val testcasecmd = new Cmd[Int](testcmd)
//        val testcase2a = new Phase2a[Int](testinst, testballot, testcasecmd, testacc);
//        if(!statesMap.contains(testcase2a))
//        {
//          statesMap += (testcase2a -> stateNum);
//          stateNum += 1;
//        }
//        val testcase2b = new Phase2b[Int](testinst, testballot, testacc, testcasecmd);
//        if(!statesMap.contains(testcase2b))
//        {
//          statesMap += (testcase2b -> stateNum);
//          stateNum += 1;
//        }
//        val testvote = new Vote[Int](testinst, testcasecmd)
//        if(!statesMap.contains(testvote))
//        {
//          statesMap += (testvote -> stateNum);
//          stateNum += 1;
//        }
//      }
////      val default_finfun:finfun[Nat, Option[(cmd[Int], Nat)]] = new finfun_const[Option[(cmd[Int], Nat)], Nat](None)
////      val lastvote:finfun[Nat, Option[(cmd[Int], Nat)]] = new finfun_update_code[Nat, Option[(cmd[Int], Nat)]](default_finfun, testinst, Some(new Cmd[Int](testcmd), testballot))
////      val testcase1b = new Phase1b(lastvote, testballot, testacc)
////      if(!statesMap.contains(testcase1b))
////      {
////        statesMap += (testcase1b -> stateNum);
////        stateNum += 1;
////      }
//      val testcasefwd = new Fwd[Int](testcmd)
//      if(!statesMap.contains(testcasefwd))
//      {
//        statesMap += (testcasefwd -> stateNum);
//        stateNum += 1;
//      }
//    }
//    println("The size of the random-tested hashMap is " + statesMap.size)
    
//    val maxAcc = 5;
//    val maxBallot = 4;
//    val maxInst = 5;
//    val maxCmd = 5;
//    var statesMap = new HashMap[packet[Int], Int]() 
//    var stateNum = 0
//    
//    val maxLoop = 1000000;
//    
//    for (acc <- 0 until maxAcc)
//    {
//      val accNat = new Nat(acc);
//      for (ballot <- 0 until maxBallot)
//      {
//        val ballotNat = new Nat(ballot);
//        val testcase1a = new Phase1a[Int](accNat, ballotNat)
//        for (inst <- 0 until maxInst)
//        {
//          val instNat = new Nat(inst);
//          for (cmd <- 0 until maxCmd)
//          {
//            val testcasefwd = new Fwd[Int](cmd)
//            val default_finfun:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_const[Option[(cmd[Int], nat)],nat](None)
//            val lastvote:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_update_code[nat, Option[(cmd[Int], nat)]](default_finfun, instNat, Some(new Cmd[Int](cmd), ballotNat))
//            val testcase1b = new Phase1b[Int](lastvote, ballotNat, accNat)
//            val testcmd = new Cmd[Int](cmd)  
//            val testcase2a = new Phase2a[Int](instNat, ballotNat, testcmd, accNat);
//            val testcase2b = new Phase2b[Int](instNat, ballotNat, accNat, testcmd);
//            val testvote = new Vote[Int](instNat, testcmd)
//            for(iacc <- 0 until maxAcc)
//            {
//              val newpacket1a = new Packet[Int](accNat, new Nat(iacc), testcase1a)
//              if(!statesMap.contains(newpacket1a))
//              {
//                statesMap += (newpacket1a -> stateNum);
//                stateNum += 1;
//              }
//              val newpacket2a = new Packet[Int](accNat, new Nat(iacc), testcase2a)
//              if(!statesMap.contains(newpacket2a))
//              {
//                statesMap += (newpacket2a -> stateNum);
//                stateNum += 1;
//              }
//              val newpacket2b = new Packet[Int](accNat, new Nat(iacc), testcase2b)
//              if(!statesMap.contains(newpacket2b))
//              {
//                statesMap += (newpacket2b -> stateNum);
//                stateNum += 1;
//              }
//              val newpacketvote = new Packet[Int](accNat, new Nat(iacc), testvote)
//              if(!statesMap.contains(newpacketvote))
//              {
//                statesMap += (newpacketvote -> stateNum);
//                stateNum += 1;
//              }
//              val newpacketfwd = new Packet[Int](accNat, new Nat(iacc), testcasefwd)
//              if(!statesMap.contains(newpacketfwd))
//              {
//                statesMap += (newpacketfwd -> stateNum);
//                stateNum += 1;
//              }
//              val newpacket1b = new Packet[Int](accNat, new Nat(iacc), testcase1b)
//              if(!statesMap.contains(newpacket1b))
//              {
//                statesMap += (newpacket1b -> stateNum);
//                stateNum += 1;
//              }
//            }
//          }
//        }
//      }
//    }
//    println("The size of the constructed hashMap is " + statesMap.size)
//   
//    val accrandom = new scala.util.Random
//    val ballotrandom = new scala.util.Random
//    val cmdrandom = new scala.util.Random
//    val instrandom = new scala.util.Random
//    val newaccrandom = new scala.util.Random
//    for(i <- 0 to maxLoop)
//    {
//      val testacc = new Nat(accrandom.nextInt(maxAcc));
//      val testballot = new Nat(ballotrandom.nextInt(maxBallot));
//      val testcmd = cmdrandom.nextInt(maxCmd);
//      val testinst = new Nat(instrandom.nextInt(maxInst));
//      val testnewacc = new Nat(newaccrandom.nextInt(maxAcc));
//      
//      val testcase1a = new Phase1a[Int](testacc, testballot)
//      
//      val newpacket1a = new Packet[Int](testacc, testnewacc, testcase1a)
//      if(!statesMap.contains(newpacket1a))
//      {
//        statesMap += (newpacket1a -> stateNum);
//        stateNum += 1;
//      }
//      val testcasefwd = new Fwd[Int](testcmd)
//      val newpacketfwd = new Packet[Int](testacc, testnewacc, testcasefwd)
//      if(!statesMap.contains(newpacketfwd))
//      {
//        statesMap += (newpacketfwd -> stateNum);
//        stateNum += 1;
//      }
//      val default_finfun:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_const[Option[(cmd[Int], nat)],nat](None)
//      val lastvote:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_update_code[nat, Option[(cmd[Int], nat)]](default_finfun, testinst, Some(new Cmd[Int](testcmd), testballot))
//      val testcase1b = new Phase1b[Int](lastvote, testballot, testacc)
//      val newpacket1b = new Packet[Int](testacc, testnewacc, testcase1b)
//      if(!statesMap.contains(newpacket1b))
//      {
//        statesMap += (newpacket1b -> stateNum);
//        stateNum += 1;
//      }
//      val testcasecmd = new Cmd[Int](testcmd)
//      val testcase2a = new Phase2a[Int](testinst, testballot, testcasecmd, testacc);
//      val testcase2b = new Phase2b[Int](testinst, testballot, testacc, testcasecmd);
//      val testvote = new Vote[Int](testinst, testcasecmd)
//      val newpacket2a = new Packet[Int](testacc, testnewacc, testcase2a)
//      if(!statesMap.contains(newpacket2a))
//      {
//        statesMap += (newpacket2a -> stateNum);
//        stateNum += 1;
//      }
//      val newpacket2b = new Packet[Int](testacc, testnewacc, testcase2b)
//      if(!statesMap.contains(newpacket2b))
//      {
//        statesMap += (newpacket2b -> stateNum);
//        stateNum += 1;
//      }
//      val newpacketvote = new Packet[Int](testacc, testnewacc, testvote)
//      if(!statesMap.contains(newpacketvote))
//      {
//        statesMap += (newpacketvote -> stateNum);
//        stateNum += 1;
//      }
//
//    }
//    println("The size of the random-tested hashMap is " + statesMap.size)
    
//    val maxAcc = 5;
//    val maxBallot = 4;
//    val maxInst = 5;
//    val maxCmd = 5;
//    val msgTypeNum = 6;
//    var statesMap = new HashMap[fset[packet[Int]], Int]() 
//    var stateNum = 0
//    
//    val listcard0:fset[packet[Int]] = new Abs_fset[packet[Int]](new seta(List()))
//    statesMap += (listcard0 -> stateNum)
//    stateNum += 1;
//    
//    val maxLoop = 100000;
//
//    for (acc <- 0 until maxAcc)
//    {
//      val accNat = new Nat(acc);
//      for (ballot <- 0 until maxBallot)
//      {
//        val ballotNat = new Nat(ballot);
//        val msg1a = new Phase1a[Int](accNat, ballotNat)
//        for (inst <- 0 until maxInst)
//        {
//          val instNat = new Nat(inst);
//          for (cmd <- 0 until maxCmd)
//          {
//            val msgfwd = new Fwd[Int](cmd)
//            val default_finfun:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_const[Option[(cmd[Int], nat)],nat](None)
//            val lastvote:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_update_code[nat, Option[(cmd[Int], nat)]](default_finfun, instNat, Some(new Cmd[Int](cmd), ballotNat))
//            val msg1b = new Phase1b[Int](lastvote, ballotNat, accNat)
//            val cmdNat = new Cmd[Int](cmd)
//            val msg2a = new Phase2a[Int](instNat, ballotNat, cmdNat, accNat)
//            val msg2b = new Phase2b[Int](instNat, ballotNat, accNat, cmdNat)
//            val msgvote = new Vote[Int](instNat, cmdNat)
//            for(iacc <- 0 until maxAcc)
//            {
//              val newpacket1a = new Packet[Int](accNat, new Nat(iacc), msg1a)
//              val newpacket1b = new Packet[Int](accNat, new Nat(iacc), msg1b)
//              val newpacket2a = new Packet[Int](accNat, new Nat(iacc), msg2a)
//              val newpacket2b = new Packet[Int](accNat, new Nat(iacc), msg2b)
//              val newpacketvote = new Packet[Int](accNat, new Nat(iacc), msgvote)
//              val newpacketfwd = new Packet[Int](accNat, new Nat(iacc), msgfwd)
//              
//              val packetlist = List(newpacket1a, newpacket1b, newpacket2a, newpacket2b, newpacketvote, newpacketfwd)
//              for(index <- 0 until msgTypeNum)
//              {
//                val list1:fset[packet[Int]] = new Abs_fset[packet[Int]](new seta(List(packetlist(index))))
//                if(!statesMap.contains(list1))
//                {
//                  statesMap += (list1 -> stateNum);
//                  stateNum += 1;
//                }
//                for(iindex <- 1 until msgTypeNum)
//                {
//                  val list2:fset[packet[Int]] = new Abs_fset[packet[Int]](new seta(List(packetlist(index), packetlist((index + iindex) % msgTypeNum))))
//                  if(!statesMap.contains(list2))
//                  {
//                    statesMap += (list2 -> stateNum);
//                    stateNum += 1;
//                  }
//                  for(iiindex <- 1 until (msgTypeNum - 1))
//                  {
//                    val list3:fset[packet[Int]] = new Abs_fset[packet[Int]](new seta(List(packetlist(index), packetlist((index + iindex) % msgTypeNum), packetlist((index + iindex + iiindex) % msgTypeNum))))
//                    if(!statesMap.contains(list3))
//                    {
//                      statesMap += (list3 -> stateNum);
//                      stateNum += 1;
//                    }
//                  }
//                }
//              }
//            }
//          }
//        }
//      }
//    }
//    println("The size of the constructed hashMap is " + statesMap.size)
//   
//    val accrandom = new scala.util.Random
//    val ballotrandom = new scala.util.Random
//    val cmdrandom = new scala.util.Random
//    val instrandom = new scala.util.Random
//    val newaccrandom = new scala.util.Random
//    for(i <- 0 to maxLoop)
//    {
//      val testacc = new Nat(accrandom.nextInt(maxAcc));
//      val testballot = new Nat(ballotrandom.nextInt(maxBallot));
//      val testcmd = cmdrandom.nextInt(maxCmd);
//      val testinst = new Nat(instrandom.nextInt(maxInst));
//      val testnewacc = new Nat(newaccrandom.nextInt(maxAcc));
//
//      val default_finfun:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_const[Option[(cmd[Int], nat)],nat](None)
//      val lastvote:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_update_code[nat, Option[(cmd[Int], nat)]](default_finfun, testinst, Some(new Cmd[Int](testcmd), testballot))
//      
//      val testcasecmd = new Cmd[Int](testcmd)
//      
//      val msg1a = new Phase1a[Int](testacc, testballot)
//      val msg1b = new Phase1b[Int](lastvote, testballot, testacc)
//      val msg2a = new Phase2a[Int](testinst, testballot, testcasecmd, testacc);
//      val msg2b = new Phase2b[Int](testinst, testballot, testacc, testcasecmd);
//      val msgvote = new Vote[Int](testinst, testcasecmd)
//      val msgfwd = new Fwd[Int](testcmd)
//      
//      val newpacket1a = new Packet[Int](testacc, testnewacc, msg1a)
//      val newpacket1b = new Packet[Int](testacc, testnewacc, msg1b)
//      val newpacket2a = new Packet[Int](testacc, testnewacc, msg2a)
//      val newpacket2b = new Packet[Int](testacc, testnewacc, msg2b)
//      val newpacketvote = new Packet[Int](testacc, testnewacc, msgvote)
//      val newpacketfwd = new Packet[Int](testacc, testnewacc, msgfwd)
//      
//      val packetlist = List(newpacket1a, newpacket1b, newpacket2a, newpacket2b, newpacketvote, newpacketfwd)
//      for(index <- 0 until 6)
//      {
//        val list1:fset[packet[Int]] = new Abs_fset[packet[Int]](new seta(List(packetlist(index))))
//        if(!statesMap.contains(list1))
//        {
//          statesMap += (list1 -> stateNum);
//          stateNum += 1;
//        }
//        for(iindex <- 1 until msgTypeNum)
//        {
//          val list2:fset[packet[Int]] = new Abs_fset[packet[Int]](new seta(List(packetlist(index), packetlist((index + iindex) % msgTypeNum))))
//          if(!statesMap.contains(list2))
//          {
//            statesMap += (list2 -> stateNum);
//            stateNum += 1;
//          }
//          for(iiindex <- 1 until (msgTypeNum - 1))
//          {
//            val list3:fset[packet[Int]] = new Abs_fset[packet[Int]](new seta(List(packetlist(index), packetlist((index + iindex) % msgTypeNum), packetlist((index + iindex + iiindex) % msgTypeNum))))
//            if(!statesMap.contains(list3))
//            {
//              statesMap += (list3 -> stateNum);
//              stateNum += 1;
//            }
//          }
//        }
//      }
//    }
//    println("The size of the random-tested hashMap is " + statesMap.size)
    val maxAcc = 5;
    val maxBallot = 4;
    val maxInst = 5;
    val maxCmd = 5;
    val msgTypeNum = 6;
    var statesMap = new HashMap[finfun[Nat, finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]]], Int]() 
    var stateNum = 0

    val msg1blist0:finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]] = new finfun_const[List[(Nat, Option[(Cmd[Int], Nat)])],Nat](List())
    val listcard0:finfun[Nat, finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]]] = new finfun_const[finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]],Nat](msg1blist0)
    statesMap += (listcard0 -> stateNum)
    stateNum += 1;
    
    val maxLoop = 100000;

    for (acc <- 0 until maxAcc)
    {
      val accNat = new Nat(acc);
      val msg1blist: List[(Nat, Option[(Cmd[Int], Nat)])] = List((accNat, None))
      for (ballot <- 0 until maxBallot)
      {
        val ballotNat = new Nat(ballot);
        val msg1a = new Phase1a[Int](accNat, ballotNat)
        for (inst <- 0 until maxInst)
        {
          val instNat = new Nat(inst);
          for (cmd <- 0 until maxCmd)
          { 
            val fDefault1:finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]] = new finfun_const[List[(Nat, Option[(Cmd[Int], Nat)])],Nat](List())
            val fUpdate1:finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]] = new finfun_update_code(fDefault1, instNat, List((accNat, Some(new Cmd[Int](cmd), ballotNat))))
            val fDefault11:finfun[Nat, finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]]] = new finfun_const[finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]],Nat](fDefault1)
            val fUpdate11:finfun[Nat, finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]]] = new finfun_update_code(fDefault11, ballotNat, fUpdate1)
            if(!statesMap.contains(fUpdate11))
            {
              statesMap += (fUpdate11 -> stateNum);
              stateNum += 1;
            }
          }
        }
      }
    }
    println("The size of the constructed hashMap is " + statesMap.size)
   
    val accrandom = new scala.util.Random
    val ballotrandom = new scala.util.Random
    val cmdrandom = new scala.util.Random
    val instrandom = new scala.util.Random
    val newaccrandom = new scala.util.Random
    for(i <- 0 to maxLoop)
    {
      val testacc = new Nat(accrandom.nextInt(maxAcc));
      val testballot = new Nat(ballotrandom.nextInt(maxBallot));
      val testcmd = cmdrandom.nextInt(maxCmd);
      val testinst = new Nat(instrandom.nextInt(maxInst));

      val default_finfun:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_const[Option[(cmd[Int], nat)],nat](None)
      val lastvote:finfun[nat, Option[(cmd[Int], nat)]] = new finfun_update_code[nat, Option[(cmd[Int], nat)]](default_finfun, testinst, Some(new Cmd[Int](testcmd), testballot))
      
      val testcasecmd = new Cmd[Int](testcmd)
      
      val fDefault1:finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]] = new finfun_const[List[(Nat, Option[(Cmd[Int], Nat)])],Nat](List())
      val fUpdate1:finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]] = new finfun_update_code(fDefault1, testinst, List((testacc, Some(new Cmd[Int](testcmd), testballot))))
      val fDefault2:finfun[Nat, finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]]] = new finfun_const[finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]],Nat](fDefault1)
      val fUpdate2:finfun[Nat, finfun[Nat, List[(Nat, Option[(Cmd[Int], Nat)])]]] = new finfun_update_code(fDefault2, testballot, fUpdate1)

      if(!statesMap.contains(fUpdate2))
      {
        statesMap += (fUpdate2 -> stateNum);
        stateNum += 1;
      }
    }
    println("The size of the random-tested hashMap is " + statesMap.size)
  }
}