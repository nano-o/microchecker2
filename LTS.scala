package microchecker

trait LTS[S,L] 
{
 def initialStates : Set[S]
 def successors(s:S) : Set[(L,S)]
 def invariants : Set[S => Boolean]
 def constraints: Set[S => Boolean]
}

