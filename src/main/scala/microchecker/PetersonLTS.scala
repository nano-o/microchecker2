package microchecker

abstract class Process {
  def getOtherFlag(s : State) : Boolean
  def setFlag(s : State, b: Boolean) : Unit
  def getStep(s : State) : Int
  def setStep(s : State, step: Int) : Unit
  def isTurn(s : State) = s.turn == this
  def giveTurn(s : State) : Unit
}
case object P1 extends Process {
  override def getOtherFlag(s : State) = s.flag2
  override def setFlag(s : State, b : Boolean) = s.flag1 = b
  override def getStep(s : State) = s.step1
  override def setStep(s : State, step : Int) = s.step1 = step
  override def giveTurn(s : State) = s.turn = P2
}
case object P2 extends Process{
  override def getOtherFlag(s : State) = s.flag1
  override def setFlag(s : State, b : Boolean) = s.flag2 = b
  override def getStep(s : State) = s.step2
  override def setStep(s : State, step : Int) = s.step2 = step
  override def giveTurn(s : State) = s.turn = P1
}

class State(p: Process)
{
  var flag1 = false;
  var flag2 = false;
  var turn = p;
  var step1 = 0;
  var step2 = 0;
  
  def copy(flag1 : Boolean = flag1, flag2 : Boolean = flag2, turn : Process = turn, step1 : Int = step1, step2 : Int = step2) : State = 
  {
    var newState = new State(this.turn)
    newState.flag1 = this.flag1
    newState.flag2 = this.flag2
    newState.turn = this.turn
    newState.step1 = this.step1
    newState.step2 = this.step2
    newState
  }
  
  def canEqual(other: Any) = other.isInstanceOf[State]
  
  override def equals(other:Any) = other match {
    case that:State => (that canEqual this) && this.flag1 == that.flag1 && this.flag2 == that.flag2 && this.turn == that.turn && this.step1 == that.step1 && this.step2 == that.step2
    case _ => false
  }

  override def toString = "flag = (" + flag1.toString() + ", " + flag2.toString() + "), turn = " + turn.toString() + ", step = (" + step1.toString() + ", " + step2.toString() + ")"
  
	override def hashCode: Int = 
	41 * (
			41 * (
					41 * (
							41 * (          
									41 + flag1.hashCode
									) + flag2.hashCode
							) + turn.hashCode
					) + step1.hashCode
			) + step2.hashCode
}

class PetersonLTS extends LTS[State, Unit]
{
  
  def process(s: State, p: Process) : Set[(Unit,State)] = 
  {
    val step = p.getStep(s)
    var newState = s.copy()
    if(step == 0)
    {
      p.setFlag(newState, true)
      p.setStep(newState, 1)
    }
    if(step == 1)
    {
      p.setStep(newState, 2)
      p.giveTurn(newState)
    }
    if(step == 2)
    {
      if ( !(p.getOtherFlag(newState) && !p.isTurn(newState)) )
      {
        p.setStep(newState, 3)
      }
    }
    if(step == 3)
    {
      p.setStep(newState, 4)
    }
    if(step == 4)
    {
      p.setFlag(newState, false)
      p.setStep(newState, 5)
    }
    if(step == 5)
    {
      p.setStep(newState, 0)
    }
    
    val trans = ((),newState) 
    Set(trans);
  }
  
  override def initialStates = Set(new State(P1), new State(P2));
  
  override def successors(s: State) : Set[(Unit,State)] = 
  {
    var newStates : Set[(Unit,State)] = Set();
    
    newStates ++= process(s, P1)
    newStates ++= process(s, P2)
    
    newStates
  }
  
  override def invariants = Set(s => (!(s.step1 == 3 & s.step2 == 3)));
  
  override def constraints = Set();
  
}