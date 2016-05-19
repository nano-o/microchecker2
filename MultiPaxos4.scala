package microchecker

object MicroCheckerLib {

abstract sealed class nat
final case class Nat(a: BigInt) extends nat
{
  override def equals(other: Any) = other match {
    case that:Nat => (that.isInstanceOf[Nat]) && this.a == that.a
    case _ => false
  }
  override def toString = a.toString()
  override def hashCode : Int = a.hashCode
}

def integer_of_nat(x0: nat): BigInt = x0 match {
  case Nat(x) => x
}

def equal_nata(m: nat, n: nat): Boolean = integer_of_nat(m) == integer_of_nat(n)

trait equal[A] {
  val `MicroCheckerLib.equal`: (A, A) => Boolean
}
def equal[A](a: A, b: A)(implicit A: equal[A]): Boolean =
  A.`MicroCheckerLib.equal`(a, b)

implicit def equal_t[A] : equal[A] = new equal[A] {
  val `MicroCheckerLib.equal` = (a : A, b: A) => a == b
}

implicit def equal_nat: equal[nat] = new equal[nat] {
  val `MicroCheckerLib.equal` = (a: nat, b: nat) => equal_nata(a, b)
}

def less_eq_nat(m: nat, n: nat): Boolean =
  integer_of_nat(m) <= integer_of_nat(n)

trait ord[A] {
  val `MicroCheckerLib.less_eq`: (A, A) => Boolean
  val `MicroCheckerLib.less`: (A, A) => Boolean
}
def less_eq[A](a: A, b: A)(implicit A: ord[A]): Boolean =
  A.`MicroCheckerLib.less_eq`(a, b)
def less[A](a: A, b: A)(implicit A: ord[A]): Boolean =
  A.`MicroCheckerLib.less`(a, b)

def less_nat(m: nat, n: nat): Boolean = integer_of_nat(m) < integer_of_nat(n)

implicit def ord_nat: ord[nat] = new ord[nat] {
  val `MicroCheckerLib.less_eq` = (a: nat, b: nat) => less_eq_nat(a, b)
  val `MicroCheckerLib.less` = (a: nat, b: nat) => less_nat(a, b)
}

trait preorder[A] extends ord[A] {
}

trait order[A] extends preorder[A] {
}

implicit def preorder_nat: preorder[nat] = new preorder[nat] {
  val `MicroCheckerLib.less_eq` = (a: nat, b: nat) => less_eq_nat(a, b)
  val `MicroCheckerLib.less` = (a: nat, b: nat) => less_nat(a, b)
}

implicit def order_nat: order[nat] = new order[nat] {
  val `MicroCheckerLib.less_eq` = (a: nat, b: nat) => less_eq_nat(a, b)
  val `MicroCheckerLib.less` = (a: nat, b: nat) => less_nat(a, b)
}

trait linorder[A] extends order[A] {
}

implicit def linorder_nat: linorder[nat] = new linorder[nat] {
  val `MicroCheckerLib.less_eq` = (a: nat, b: nat) => less_eq_nat(a, b)
  val `MicroCheckerLib.less` = (a: nat, b: nat) => less_nat(a, b)
}

abstract sealed class phantom[A, B]
final case class phantoma[B, A](a: B) extends phantom[A, B]
{
  override def equals(other: Any) = other match {
    case that:phantoma[B,A] => (that.isInstanceOf[phantoma[B,A]]) && this.a == that.a
    case _ => false
  }
  override def toString = "phantoma(" + a.toString() + ")"
  override def hashCode : Int = a.hashCode
}

def finite_UNIV_nata: phantom[nat, Boolean] = phantoma[Boolean, nat](false)

def zero_nat: nat = Nat(BigInt(0))

def card_UNIV_nata: phantom[nat, nat] = phantoma[nat, nat](zero_nat)

trait finite_UNIV[A] {
  val `MicroCheckerLib.finite_UNIV`: phantom[A, Boolean]
}
def finite_UNIV[A](implicit A: finite_UNIV[A]): phantom[A, Boolean] =
  A.`MicroCheckerLib.finite_UNIV`

trait card_UNIV[A] extends finite_UNIV[A] {
  val `MicroCheckerLib.card_UNIVa`: phantom[A, nat]
}
def card_UNIVa[A](implicit A: card_UNIV[A]): phantom[A, nat] =
  A.`MicroCheckerLib.card_UNIVa`

implicit def finite_UNIV_nat: finite_UNIV[nat] = new finite_UNIV[nat] {
  val `MicroCheckerLib.finite_UNIV` = finite_UNIV_nata
}

implicit def card_UNIV_nat: card_UNIV[nat] = new card_UNIV[nat] {
  val `MicroCheckerLib.card_UNIVa` = card_UNIV_nata
  val `MicroCheckerLib.finite_UNIV` = finite_UNIV_nata
}

def equal_boola(p: Boolean, pa: Boolean): Boolean = (p, pa) match {
  case (p, true) => p
  case (p, false) => ! p
  case (true, p) => p
  case (false, p) => ! p
}

implicit def equal_bool: equal[Boolean] = new equal[Boolean] {
  val `MicroCheckerLib.equal` = (a: Boolean, b: Boolean) => equal_boola(a, b)
}

def eq[A : equal](a: A, b: A): Boolean = equal[A](a, b)

def equal_lista[A : equal](x0: List[A], x1: List[A]): Boolean = (x0, x1) match {
  case (Nil, x21 :: x22) => false
  case (x21 :: x22, Nil) => false
  case (x21 :: x22, y21 :: y22) => eq[A](x21, y21) && equal_lista[A](x22, y22)
  case (Nil, Nil) => true
}

implicit def equal_list[A : equal]: equal[List[A]] = new equal[List[A]] {
  val `MicroCheckerLib.equal` = (a: List[A], b: List[A]) => equal_lista[A](a, b)
}

abstract sealed class finfun[A, B]
final case class finfun_const[B, A](a: B) extends finfun[A, B]
{
  override def equals(other: Any) = other match {
    case that:finfun_const[B,A] => (that.isInstanceOf[finfun_const[B,A]]) && this.a == that.a
    case _ => false
  }
  override def toString = "[default |-> " + a.toString() + "]"
  override def hashCode : Int = a.hashCode
}
final case class finfun_update_code[A, B](a: finfun[A, B], b: A, c: B) extends
  finfun[A, B]
{
  override def equals(other: Any) = other match {
    case that:finfun_update_code[A,B] => (that.isInstanceOf[finfun_update_code[A,B]]) && (finfun_to_set(this) == finfun_to_set(that) && finfun_constv(this) == finfun_constv(that))
    case _ => false
  }
  override def toString = "[" + b.toString() + " |-> " + c.toString() + "]" + a.toString()
  override def hashCode : Int =   41 * ( 41 + finfun_constv(this).hashCode()) + finfun_to_set(this).hashCode()
}


def finfun_to_set[A, B](x0: finfun[A, B]): Set[(A,B)] = x0 match {
  case finfun_update_code(f, a, b) => (
    if (eq[B](b, finfun_constv[A, B](f)))
      finfun_to_set[A, B](f) - Tuple2(a,b)
    else
      finfun_to_set[A, B](f)) + Tuple2(a,b)
  case finfun_const(c) => Set()
}

def finfun_constv[A, B](x0: finfun[A, B]): B = x0 match {
  case finfun_update_code(f, a, b) => finfun_constv[A, B](f)
  case finfun_const(c) => c
}

def finfun_comp[A, B, C](g: A => B, x1: finfun[C, A]): finfun[C, B] = (g, x1)
  match {
  case (g, finfun_update_code(f, a, b)) =>
    finfun_update_code[C, B](finfun_comp[A, B, C](g, f), a, g(b))
  case (g, finfun_const(c)) => finfun_const[B, C](g(c))
}

def finfun_update[A : equal, B : equal](x0: finfun[A, B], a: A, b: B):
      finfun[A, B]
  =
  (x0, a, b) match {
  case (finfun_update_code(f, aa, ba), a, b) =>
    (if (eq[A](aa, a)) finfun_update[A, B](f, aa, b)
      else finfun_update_code[A, B](finfun_update[A, B](f, a, b), aa, ba))
  case (finfun_const(ba), a, b) =>
    (if (eq[B](ba, b)) finfun_const[B, A](ba)
      else finfun_update_code[A, B](finfun_const[B, A](ba), a, b))
}

def finfun_apply[A : equal, B](x0: finfun[A, B], a: A): B = (x0, a) match {
  case (finfun_const(b), a) => b
  case (finfun_update_code(f, aa, b), a) =>
    (if (eq[A](aa, a)) b else finfun_apply[A, B](f, a))
}

def equal_proda[A : equal, B : equal](x0: (A, B), x1: (A, B)): Boolean =
  (x0, x1) match {
  case ((x1, x2), (y1, y2)) => eq[A](x1, y1) && eq[B](x2, y2)
}

implicit def equal_prod[A : equal, B : equal]: equal[(A, B)] = new equal[(A, B)]
  {
  val `MicroCheckerLib.equal` = (a: (A, B), b: (A, B)) =>
    equal_proda[A, B](a, b)
}

def finfun_Diag[A : equal, B : equal,
                 C : equal](x0: finfun[A, B], g: finfun[A, C]):
      finfun[A, (B, C)]
  =
  (x0, g) match {
  case (finfun_update_code(f, a, b), g) =>
    finfun_update[A, (B, C)](finfun_Diag[A, B, C](f, g), a,
                              (b, finfun_apply[A, C](g, a)))
  case (finfun_const(b), finfun_update_code(g, a, c)) =>
    finfun_update_code[A, (B, C)](finfun_Diag[A, B,
       C](finfun_const[B, A](b), g),
                                   a, (b, c))
  case (finfun_const(b), finfun_const(c)) => finfun_const[(B, C), A]((b, c))
}

def of_phantom[A, B](x0: phantom[A, B]): B = x0 match {
  case phantoma(x) => x
}

def plus_nat(m: nat, n: nat): nat = Nat(integer_of_nat(m) + integer_of_nat(n))

abstract sealed class num
final case class One() extends num
{
  override def equals(other: Any) = other match {
    case that:One => (that.isInstanceOf[One])
    case _ => false
  }
  override def toString = "One"
  override def hashCode : Int = 41
}
final case class Bit0(a: num) extends num
{
  override def equals(other: Any) = other match {
    case that:Bit0 => (that.isInstanceOf[Bit0]) && this.a == that.a
    case _ => false
  }
  override def toString = "Bit0(" + a.toString() + ")"
  override def hashCode : Int = a.hashCode
}
final case class Bit1(a: num) extends num
{
  override def equals(other: Any) = other match {
    case that:Bit1 => (that.isInstanceOf[Bit1]) && this.a == that.a
    case _ => false
  }
  override def toString = "Bit1(" + a.toString() + ")"
  override def hashCode : Int = a.hashCode
}

def one_nat: nat = Nat(BigInt(1))

def Suc(n: nat): nat = plus_nat(n, one_nat)

def gen_length[A](n: nat, x1: List[A]): nat = (n, x1) match {
  case (n, x :: xs) => gen_length[A](Suc(n), xs)
  case (n, Nil) => n
}

def size_list[A]: (List[A]) => nat =
  ((a: List[A]) => gen_length[A](zero_nat, a))

def card_UNIV[A : card_UNIV]: phantom[A, nat] = card_UNIVa[A]

def membera[A : equal](x0: List[A], y: A): Boolean = (x0, y) match {
  case (Nil, y) => false
  case (x :: xs, y) => eq[A](x, y) || membera[A](xs, y)
}

def remdups[A : equal](x0: List[A]): List[A] = x0 match {
  case Nil => Nil
  case x :: xs =>
    (if (membera[A](xs, x)) remdups[A](xs) else x :: remdups[A](xs))
}

def is_list_UNIV[A : card_UNIV : equal](xs: List[A]): Boolean =
  {
    val c: nat = of_phantom[A, nat](card_UNIV[A]);
    (if (equal_nata(c, zero_nat)) false
      else equal_nata(size_list[A].apply(remdups[A](xs)), c))
  }

def finfun_All_except[A : card_UNIV : equal](aa: List[A],
      x1: finfun[A, Boolean]):
      Boolean
  =
  (aa, x1) match {
  case (aa, finfun_update_code(f, a, b)) =>
    (membera[A](aa, a) || b) && finfun_All_except[A](a :: aa, f)
  case (a, finfun_const(b)) => b || is_list_UNIV[A](a)
}

def finfun_All[A : card_UNIV : equal]: (finfun[A, Boolean]) => Boolean =
  ((a: finfun[A, Boolean]) => finfun_All_except[A](Nil, a))

def equal_finfuna[A : card_UNIV : equal,
                   B : equal](f: finfun[A, B], g: finfun[A, B]):
      Boolean
  =
  finfun_All[A].apply(finfun_comp[(B, B), Boolean,
                                   A](((a: (B, B)) =>
{
  val (aa, b): (B, B) = a;
  eq[B](aa, b)
}),
                                       finfun_Diag[A, B, B](f, g)))

implicit def equal_finfun[A : card_UNIV : equal, B : equal]: equal[finfun[A, B]]
  = new equal[finfun[A, B]] {
  val `MicroCheckerLib.equal` = (a: finfun[A, B], b: finfun[A, B]) =>
    equal_finfuna[A, B](a, b)
}

def less_eq_o[A : ord](x0: Option[A], uu: Option[A]): Boolean = (x0, uu) match {
  case (None, uu) => true
  case (Some(x), None) => false
  case (Some(x), Some(y)) => less_eq[A](x, y)
}

def less_eq_option[A : preorder](o1: Option[A], o2: Option[A]): Boolean =
  less_eq_o[A](o1, o2)

def less_o[A : ord](x0: Option[A], x1: Option[A]): Boolean = (x0, x1) match {
  case (None, None) => false
  case (None, Some(v)) => true
  case (Some(x), None) => false
  case (Some(x), Some(y)) => less[A](x, y)
}

def less_option[A : preorder](o1: Option[A], o2: Option[A]): Boolean =
  less_o[A](o1, o2)

implicit def ord_option[A : preorder]: ord[Option[A]] = new ord[Option[A]] {
  val `MicroCheckerLib.less_eq` = (a: Option[A], b: Option[A]) =>
    less_eq_option[A](a, b)
  val `MicroCheckerLib.less` = (a: Option[A], b: Option[A]) =>
    less_option[A](a, b)
}

implicit def preorder_option[A : preorder]: preorder[Option[A]] = new
  preorder[Option[A]] {
  val `MicroCheckerLib.less_eq` = (a: Option[A], b: Option[A]) =>
    less_eq_option[A](a, b)
  val `MicroCheckerLib.less` = (a: Option[A], b: Option[A]) =>
    less_option[A](a, b)
}

implicit def order_option[A : linorder]: order[Option[A]] = new order[Option[A]]
  {
  val `MicroCheckerLib.less_eq` = (a: Option[A], b: Option[A]) =>
    less_eq_option[A](a, b)
  val `MicroCheckerLib.less` = (a: Option[A], b: Option[A]) =>
    less_option[A](a, b)
}

implicit def linorder_option[A : linorder]: linorder[Option[A]] = new
  linorder[Option[A]] {
  val `MicroCheckerLib.less_eq` = (a: Option[A], b: Option[A]) =>
    less_eq_option[A](a, b)
  val `MicroCheckerLib.less` = (a: Option[A], b: Option[A]) =>
    less_option[A](a, b)
}

def equal_unita(u: Unit, v: Unit): Boolean = true

implicit def equal_unit: equal[Unit] = new equal[Unit] {
  val `MicroCheckerLib.equal` = (a: Unit, b: Unit) => equal_unita(a, b)
}

implicit def ord_integer: ord[BigInt] = new ord[BigInt] {
  val `MicroCheckerLib.less_eq` = (a: BigInt, b: BigInt) => a <= b
  val `MicroCheckerLib.less` = (a: BigInt, b: BigInt) => a < b
}

abstract sealed class set[A]
final case class seta[A](a: List[A]) extends set[A]
{
  override def equals(other: Any) = other match {
    case that:seta[A] => (that.isInstanceOf[seta[A]]) && (Set(a) == Set(that.a))
    case _ => false
  }
  override def toString = a.mkString("{",",","}")
  override def hashCode : Int = Set(a).hashCode
}
final case class coset[A](a: List[A]) extends set[A]
{
  override def equals(other: Any) = other match {
    case that:coset[A] => (that.isInstanceOf[coset[A]]) && (Set(a) == Set(that.a))
    case _ => false
  }
  override def toString = a.mkString("{",",","}")
  override def hashCode : Int = Set(a).hashCode
}

abstract sealed class fset[A]
final case class Abs_fset[A](a: set[A]) extends fset[A]
{
  override def equals(other: Any) = other match {
    case that:Abs_fset[A] => (that.isInstanceOf[Abs_fset[A]]) && this.a == that.a
    case _ => false
  }
  override def toString = a.toString()
  override def hashCode : Int = a.hashCode
}

abstract sealed class nibble
final case class Nibble0() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble0 => (that.isInstanceOf[Nibble0])
    case _ => false
  }
  override def toString = "Nibble0"
  override def hashCode : Int = 41
}
final case class Nibble1() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble1 => (that.isInstanceOf[Nibble1])
    case _ => false
  }
  override def toString = "Nibble1"
  override def hashCode : Int = 41
}
final case class Nibble2() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble2 => (that.isInstanceOf[Nibble2])
    case _ => false
  }
  override def toString = "Nibble2"
  override def hashCode : Int = 41
}
final case class Nibble3() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble3 => (that.isInstanceOf[Nibble3])
    case _ => false
  }
  override def toString = "Nibble3"
  override def hashCode : Int = 41
}
final case class Nibble4() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble4 => (that.isInstanceOf[Nibble4])
    case _ => false
  }
  override def toString = "Nibble4"
  override def hashCode : Int = 41
}
final case class Nibble5() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble5 => (that.isInstanceOf[Nibble5])
    case _ => false
  }
  override def toString = "Nibble5"
  override def hashCode : Int = 41
}
final case class Nibble6() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble6 => (that.isInstanceOf[Nibble6])
    case _ => false
  }
  override def toString = "Nibble6"
  override def hashCode : Int = 41
}
final case class Nibble7() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble7 => (that.isInstanceOf[Nibble7])
    case _ => false
  }
  override def toString = "Nibble7"
  override def hashCode : Int = 41
}
final case class Nibble8() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble8 => (that.isInstanceOf[Nibble8])
    case _ => false
  }
  override def toString = "Nibble8"
  override def hashCode : Int = 41
}
final case class Nibble9() extends nibble
{
  override def equals(other: Any) = other match {
    case that:Nibble9 => (that.isInstanceOf[Nibble9])
    case _ => false
  }
  override def toString = "Nibble9"
  override def hashCode : Int = 41
}
final case class NibbleA() extends nibble
{
  override def equals(other: Any) = other match {
    case that:NibbleA => (that.isInstanceOf[NibbleA])
    case _ => false
  }
  override def toString = "NibbleA"
  override def hashCode : Int = 41
}
final case class NibbleB() extends nibble
{
  override def equals(other: Any) = other match {
    case that:NibbleB => (that.isInstanceOf[NibbleB])
    case _ => false
  }
  override def toString = "NibbleB"
  override def hashCode : Int = 41
}
final case class NibbleC() extends nibble
{
  override def equals(other: Any) = other match {
    case that:NibbleC => (that.isInstanceOf[NibbleC])
    case _ => false
  }
  override def toString = "NibbleC"
  override def hashCode : Int = 41
}
final case class NibbleD() extends nibble
{
  override def equals(other: Any) = other match {
    case that:NibbleD => (that.isInstanceOf[NibbleD])
    case _ => false
  }
  override def toString = "NibbleD"
  override def hashCode : Int = 41
}
final case class NibbleE() extends nibble
{
  override def equals(other: Any) = other match {
    case that:NibbleE => (that.isInstanceOf[NibbleE])
    case _ => false
  }
  override def toString = "NibbleE"
  override def hashCode : Int = 41
}
final case class NibbleF() extends nibble
{
  override def equals(other: Any) = other match {
    case that:NibbleF => (that.isInstanceOf[NibbleF])
    case _ => false
  }
  override def toString = "NibbleF"
  override def hashCode : Int = 41
}

abstract sealed class char
final case class Char(a: nibble, b: nibble) extends char
{
  override def equals(other: Any) = other match {
    case that:Char => (that.isInstanceOf[Char]) && this.a == that.a && this.b == that.b
    case _ => false
  }
  override def toString = "Char(" + a.toString() + ", " + b.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (1) + a.hashCode) + b.hashCode
}

def id[A]: A => A = ((x: A) => x)

def comp[A, B, C](f: A => B, g: C => A): C => B = ((x: C) => f(g(x)))

def max[A : ord](a: A, b: A): A = (if (less_eq[A](a, b)) b else a)

def minus_nat(m: nat, n: nat): nat =
  Nat(max[BigInt](BigInt(0), integer_of_nat(m) - integer_of_nat(n)))

def nth[A](x0: List[A], n: nat): A = (x0, n) match {
  case (x :: xs, n) =>
    (if (equal_nata(n, zero_nat)) x else nth[A](xs, minus_nat(n, one_nat)))
}

def fold[A, B](f: A => B => B, x1: List[A], s: B): B = (f, x1, s) match {
  case (f, x :: xs, s) => fold[A, B](f, xs, (f(x))(s))
  case (f, Nil, s) => s
}

def rev[A](xs: List[A]): List[A] =
  fold[A, List[A]](((a: A) => (b: List[A]) => a :: b), xs, Nil)

def upt(i: nat, j: nat): List[nat] =
  (if (less_nat(i, j)) i :: upt(Suc(i), j) else Nil)

def pred_list[A](p: A => Boolean, x1: List[A]): Boolean = (p, x1) match {
  case (p, Nil) => true
  case (p, x :: xs) => p(x) && pred_list[A](p, xs)
}

def Ball[A](x0: set[A], p: A => Boolean): Boolean = (x0, p) match {
  case (seta(xs), p) => pred_list[A](p, xs)
}

def fset[A](x0: fset[A]): set[A] = x0 match {
  case Abs_fset(x) => x
}

def nulla[A](x0: List[A]): Boolean = x0 match {
  case Nil => true
  case x :: xs => false
}

def map[A, B](f: A => B, x1: List[A]): List[B] = (f, x1) match {
  case (f, Nil) => Nil
  case (f, x21 :: x22) => f(x21) :: map[A, B](f, x22)
}

def image[A, B](f: A => B, x1: set[A]): set[B] = (f, x1) match {
  case (f, seta(xs)) => seta[B](map[A, B](f, xs))
}

def card[A : equal](x0: set[A]): nat = x0 match {
  case coset(xs) =>
    { sys.error("card (List.coset _) requires type class instance card_UNIV");
      (((_: Unit) => card[A](coset[A](xs)))).apply(()) }
  case seta(xs) => size_list[A].apply(remdups[A](xs))
}

def fcard[A : equal](xa: fset[A]): nat = card[A](fset[A](xa))

def foldr[A, B](f: A => B => B, x1: List[A]): B => B = (f, x1) match {
  case (f, Nil) => id[B]
  case (f, x :: xs) => comp[B, B, B](f(x), foldr[A, B](f, xs))
}

def removeAll[A : equal](x: A, xa1: List[A]): List[A] = (x, xa1) match {
  case (x, Nil) => Nil
  case (x, y :: xs) =>
    (if (eq[A](x, y)) removeAll[A](x, xs) else y :: removeAll[A](x, xs))
}

def inserta[A : equal](x: A, xs: List[A]): List[A] =
  (if (membera[A](xs, x)) xs else x :: xs)

def insert[A : equal](x: A, xa1: set[A]): set[A] = (x, xa1) match {
  case (x, coset(xs)) => coset[A](removeAll[A](x, xs))
  case (x, seta(xs)) => seta[A](inserta[A](x, xs))
}

def member[A : equal](x: A, xa1: set[A]): Boolean = (x, xa1) match {
  case (x, coset(xs)) => ! (membera[A](xs, x))
  case (x, seta(xs)) => membera[A](xs, x)
}

def remove[A : equal](x: A, xa1: set[A]): set[A] = (x, xa1) match {
  case (x, coset(xs)) => coset[A](inserta[A](x, xs))
  case (x, seta(xs)) => seta[A](removeAll[A](x, xs))
}

def fimage[B, A](xb: B => A, xc: fset[B]): fset[A] =
  Abs_fset[A](image[B, A](xb, fset[B](xc)))

def filter[A](p: A => Boolean, x1: List[A]): List[A] = (p, x1) match {
  case (p, Nil) => Nil
  case (p, x :: xs) => (if (p(x)) x :: filter[A](p, xs) else filter[A](p, xs))
}

def finsert[A : equal](xb: A, xc: fset[A]): fset[A] =
  Abs_fset[A](insert[A](xb, fset[A](xc)))

def fmember[A : equal](x: A, xc: fset[A]): Boolean = member[A](x, fset[A](xc))

def hd[A](x0: List[A]): A = x0 match {
  case x21 :: x22 => x21
}

def remove1[A : equal](x: A, xa1: List[A]): List[A] = (x, xa1) match {
  case (x, Nil) => Nil
  case (x, y :: xs) => (if (eq[A](x, y)) xs else y :: remove1[A](x, xs))
}

def carda[A : card_UNIV : equal](x0: set[A]): nat = x0 match {
  case coset(xs) =>
    minus_nat(of_phantom[A, nat](card_UNIVa[A]),
               size_list[A].apply(remdups[A](xs)))
  case seta(xs) => size_list[A].apply(remdups[A](xs))
}

def apsnd[A, B, C](f: A => B, x1: (C, A)): (C, B) = (f, x1) match {
  case (f, (x, y)) => (x, f(y))
}

def top_set[A]: set[A] = coset[A](Nil)

def finfun_default[A : card_UNIV : equal, B](x0: finfun[A, B]): B = x0 match {
  case finfun_update_code(f, a, b) => finfun_default[A, B](f)
  case finfun_const(c) =>
    (if (equal_nata(carda[A](top_set[A]), zero_nat)) c
      else sys.error("undefined"))
}

def insort_key[A, B : linorder](f: A => B, x: A, xa2: List[A]): List[A] =
  (f, x, xa2) match {
  case (f, x, Nil) => List(x)
  case (f, x, y :: ys) =>
    (if (less_eq[B](f(x), f(y))) x :: y :: ys
      else y :: insort_key[A, B](f, x, ys))
}

def insort_insert_key[A, B : equal : linorder](f: A => B, x: A, xs: List[A]):
      List[A]
  =
  (if (member[B](f(x), image[A, B](f, seta[A](xs)))) xs
    else insort_key[A, B](f, x, xs))

def finfun_to_list[A : card_UNIV : equal : linorder,
                    B : equal](x0: finfun[A, B]):
      List[A]
  =
  x0 match {
  case finfun_update_code(f, a, b) =>
    (if (eq[B](b, finfun_default[A, B](f)))
      remove1[A](a, finfun_to_list[A, B](f))
      else insort_insert_key[A, A](((x: A) => x), a, finfun_to_list[A, B](f)))
  case finfun_const(c) =>
    (if (equal_nata(carda[A](top_set[A]), zero_nat)) Nil
      else { sys.error("finfun_to_list called on finite type");
             (((_: Unit) =>
                finfun_to_list[A, B](finfun_const[B, A](c)))).apply(())
             })
}

def fst[A, B](x0: (A, B)): A = x0 match {
  case (x1, x2) => x1
}

def snd[A, B](x0: (A, B)): B = x0 match {
  case (x1, x2) => x2
}

def bot_set[A]: set[A] = seta[A](Nil)

def sup_set[A : equal](x0: set[A], a: set[A]): set[A] = (x0, a) match {
  case (coset(xs), a) =>
    coset[A](filter[A](((x: A) => ! (member[A](x, a))), xs))
  case (seta(xs), a) =>
    fold[A, set[A]](((aa: A) => (b: set[A]) => insert[A](aa, b)), xs, a)
}

def sgn_integer(k: BigInt): BigInt =
  (if (k == BigInt(0)) BigInt(0)
    else (if (k < BigInt(0)) BigInt(-1) else BigInt(1)))

def divmod_integer(k: BigInt, l: BigInt): (BigInt, BigInt) =
  (if (k == BigInt(0)) (BigInt(0), BigInt(0))
    else (if (l == BigInt(0)) (BigInt(0), k)
           else (comp[BigInt, ((BigInt, BigInt)) => (BigInt, BigInt),
                       BigInt](comp[BigInt => BigInt,
                                     ((BigInt, BigInt)) => (BigInt, BigInt),
                                     BigInt](((a: BigInt => BigInt) =>
       (b: (BigInt, BigInt)) => apsnd[BigInt, BigInt, BigInt](a, b)),
      ((a: BigInt) => (b: BigInt) => a * b)),
                                ((a: BigInt) =>
                                  sgn_integer(a)))).apply(l).apply((if (sgn_integer(k) ==
                                  sgn_integer(l))
                             ((k: BigInt) => (l: BigInt) => if (l == 0)
                               (BigInt(0), k) else
                               (k.abs /% l.abs)).apply(k).apply(l)
                             else {
                                    val (r, s): (BigInt, BigInt) =
                                      ((k: BigInt) => (l: BigInt) => if (l == 0)
(BigInt(0), k) else (k.abs /% l.abs)).apply(k).apply(l);
                                    (if (s == BigInt(0)) ((- r), BigInt(0))
                                      else ((- r) - BigInt(1), l.abs - s))
                                  }))))

def nat_of_integer(k: BigInt): nat = Nat(max[BigInt](BigInt(0), k))

def bot_fset[A]: fset[A] = Abs_fset[A](bot_set[A])

def sup_fset[A : equal](xb: fset[A], xc: fset[A]): fset[A] =
  Abs_fset[A](sup_set[A](fset[A](xb), fset[A](xc)))

def mod_integer(k: BigInt, l: BigInt): BigInt =
  snd[BigInt, BigInt](divmod_integer(k, l))

def mod_nat(m: nat, n: nat): nat =
  Nat(mod_integer(integer_of_nat(m), integer_of_nat(n)))

def sort_key[A, B : linorder](f: A => B, xs: List[A]): List[A] =
  (foldr[A, List[A]](((a: A) => (b: List[A]) => insort_key[A, B](f, a, b)),
                      xs)).apply(Nil)

def times_nat(m: nat, n: nat): nat = Nat(integer_of_nat(m) * integer_of_nat(n))

def minus_set[A : equal](a: set[A], x1: set[A]): set[A] = (a, x1) match {
  case (a, coset(xs)) => seta[A](filter[A](((x: A) => member[A](x, a)), xs))
  case (a, seta(xs)) =>
    fold[A, set[A]](((aa: A) => (b: set[A]) => remove[A](aa, b)), xs, a)
}

def less_eq_set[A : equal](a: set[A], b: set[A]): Boolean = (a, b) match {
  case (coset(xs), seta(ys)) =>
    (if (nulla[A](xs) && nulla[A](ys)) false
      else { sys.error("subset_eq (List.coset _) (List.set _) requires type class instance card_UNIV");
             (((_: Unit) =>
                less_eq_set[A](coset[A](xs), seta[A](ys)))).apply(())
             })
  case (a, coset(ys)) => pred_list[A](((y: A) => ! (member[A](y, a))), ys)
  case (seta(xs), b) => pred_list[A](((x: A) => member[A](x, b)), xs)
}

def serialize_finfun[A : card_UNIV : equal : linorder,
                      B : equal](ff: finfun[A, B]):
      List[(A, B)]
  =
  fold[A, List[(A, B)]](((k: A) =>
                          ((a: List[(A, B)]) =>
                            (k, finfun_apply[A, B](ff, k)) :: a)),
                         finfun_to_list[A, B](ff), Nil)

def less_eq_fset[A : equal](xa: fset[A], xc: fset[A]): Boolean =
  less_eq_set[A](fset[A](xa), fset[A](xc))

def equal_fset[A : equal](a: fset[A], b: fset[A]): Boolean =
  less_eq_fset[A](a, b) && less_eq_fset[A](b, a)

def minus_fset[A : equal](xb: fset[A], xc: fset[A]): fset[A] =
  Abs_fset[A](minus_set[A](fset[A](xb), fset[A](xc)))

def deserialize_finfun[A, B](l: List[(A, Option[B])]): finfun[A, Option[B]] =
  (foldr[(A, Option[B]),
          finfun[A, Option[B]]](((kv: (A, Option[B])) =>
                                  (r: finfun[A, Option[B]]) =>
                                  finfun_update_code[A,
              Option[B]](r, fst[A, Option[B]](kv), snd[A, Option[B]](kv))),
                                 l)).apply(finfun_const[Option[B], A](None))

def divide_integer(k: BigInt, l: BigInt): BigInt =
  fst[BigInt, BigInt](divmod_integer(k, l))

def divide_nat(m: nat, n: nat): nat =
  Nat(divide_integer(integer_of_nat(m), integer_of_nat(n)))

} /* object MicroCheckerLib */

object Optiona {

def equal_optiona[A : MicroCheckerLib.equal](x0: Option[A], x1: Option[A]):
      Boolean
  =
  (x0, x1) match {
  case (None, Some(x2)) => false
  case (Some(x2), None) => false
  case (Some(x2), Some(y2)) => MicroCheckerLib.eq[A](x2, y2)
  case (None, None) => true
}

implicit def
  equal_option[A : MicroCheckerLib.equal]: MicroCheckerLib.equal[Option[A]] =
  new MicroCheckerLib.equal[Option[A]] {
  val `MicroCheckerLib.equal` = (a: Option[A], b: Option[A]) =>
    equal_optiona[A](a, b)
}

def bind[A, B](x0: Option[A], f: A => Option[B]): Option[B] = (x0, f) match {
  case (None, f) => None
  case (Some(x), f) => f(x)
}

def is_none[A](x0: Option[A]): Boolean = x0 match {
  case Some(x) => false
  case None => true
}

def the[A](x0: Option[A]): A = x0 match {
  case Some(x2) => x2
}

} /* object Optiona */

object MultiPaxos4 {

import /*implicits*/ MicroCheckerLib.equal_unit,
  MicroCheckerLib.linorder_option, MicroCheckerLib.preorder_nat,
  MicroCheckerLib.ord_option, MicroCheckerLib.order_nat,
  MicroCheckerLib.equal_finfun, MicroCheckerLib.equal_prod,
  MicroCheckerLib.card_UNIV_nat, MicroCheckerLib.linorder_nat,
  Optiona.equal_option, MicroCheckerLib.equal_list, MicroCheckerLib.equal_bool,
  MicroCheckerLib.equal_nat

abstract sealed class cmd[A]
final case class Cmd[A](a: A) extends cmd[A]
{
  override def equals(other: Any) = other match {
    case that:Cmd[A] => (that.isInstanceOf[Cmd[A]]) && this.a == that.a
    case _ => false
  }
  override def toString = "Cmd(" + a.toString() + ")"
  override def hashCode : Int = a.hashCode
}
final case class NoOp[A]() extends cmd[A]
{
  override def equals(other: Any) = other match {
    case that:NoOp[A] => (that.isInstanceOf[NoOp[A]])
    case _ => false
  }
  override def toString = "NoOp"
  override def hashCode : Int = 41
}

def equal_cmda[A : MicroCheckerLib.equal](x0: cmd[A], x1: cmd[A]): Boolean =
  (x0, x1) match {
  case (Cmd(x1), NoOp()) => false
  case (NoOp(), Cmd(x1)) => false
  case (Cmd(x1), Cmd(y1)) => MicroCheckerLib.eq[A](x1, y1)
  case (NoOp(), NoOp()) => true
}

implicit def equal_cmd[A : MicroCheckerLib.equal]: MicroCheckerLib.equal[cmd[A]]
  = new MicroCheckerLib.equal[cmd[A]] {
  val `MicroCheckerLib.equal` = (a: cmd[A], b: cmd[A]) => equal_cmda[A](a, b)
}

def less_eq_pair[A, B : MicroCheckerLib.ord, C](x0: (A, B), x1: (C, B)): Boolean
  =
  (x0, x1) match {
  case ((x, y), (u, v)) => MicroCheckerLib.less_eq[B](y, v)
}

def less_eq_prod[A, B : MicroCheckerLib.order](x: (A, B), y: (A, B)): Boolean =
  less_eq_pair[A, B, A](x, y)

def less_pair[A, B : MicroCheckerLib.ord, C](x0: (A, B), x1: (C, B)): Boolean =
  (x0, x1) match {
  case ((x, y), (u, v)) => MicroCheckerLib.less[B](y, v)
}

def less_prod[A, B : MicroCheckerLib.order](x: (A, B), y: (A, B)): Boolean =
  less_pair[A, B, A](x, y)

implicit def ord_prod[A, B : MicroCheckerLib.order]: MicroCheckerLib.ord[(A, B)]
  = new MicroCheckerLib.ord[(A, B)] {
  val `MicroCheckerLib.less_eq` = (a: (A, B), b: (A, B)) =>
    less_eq_prod[A, B](a, b)
  val `MicroCheckerLib.less` = (a: (A, B), b: (A, B)) => less_prod[A, B](a, b)
}

implicit def
  preorder_prod[A, B : MicroCheckerLib.order]: MicroCheckerLib.preorder[(A, B)]
  = new MicroCheckerLib.preorder[(A, B)] {
  val `MicroCheckerLib.less_eq` = (a: (A, B), b: (A, B)) =>
    less_eq_prod[A, B](a, b)
  val `MicroCheckerLib.less` = (a: (A, B), b: (A, B)) => less_prod[A, B](a, b)
}

abstract sealed class msg[A]
final case class Phase1a[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat)
  extends msg[A]
{
  override def equals(other: Any) = other match {
    case that:Phase1a[A] => (that.isInstanceOf[Phase1a[A]]) && this.a == that.a && this.b == that.b
    case _ => false
  }
  override def toString = "Phase1a(" + a.toString() + ", " + b.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (1) + a.hashCode) + b.hashCode
}
final case class
  Phase1b[A](a: MicroCheckerLib.finfun[MicroCheckerLib.nat,
Option[(cmd[A], MicroCheckerLib.nat)]],
              b: MicroCheckerLib.nat, c: MicroCheckerLib.nat)
  extends msg[A]
{
  override def equals(other: Any) = other match {
    case that:Phase1b[A] => (that.isInstanceOf[Phase1b[A]]) && this.a == that.a && this.b == that.b && this.c == that.c
    case _ => false
  }
  override def toString = "Phase1b(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode
}
final case class
  Phase2a[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat, c: cmd[A],
              d: MicroCheckerLib.nat)
  extends msg[A]
{
  override def equals(other: Any) = other match {
    case that:Phase2a[A] => (that.isInstanceOf[Phase2a[A]]) && this.a == that.a && this.b == that.b && this.c == that.c && this.d == that.d
    case _ => false
  }
  override def toString = "Phase2a(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ", " + d.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode) + d.hashCode
}
final case class
  Phase2b[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat,
              c: MicroCheckerLib.nat, d: cmd[A])
  extends msg[A]
{
  override def equals(other: Any) = other match {
    case that:Phase2b[A] => (that.isInstanceOf[Phase2b[A]]) && this.a == that.a && this.b == that.b && this.c == that.c && this.d == that.d
    case _ => false
  }
  override def toString = "Phase2b(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ", " + d.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode) + d.hashCode
}
final case class Vote[A](a: MicroCheckerLib.nat, b: cmd[A]) extends msg[A]
{
  override def equals(other: Any) = other match {
    case that:Vote[A] => (that.isInstanceOf[Vote[A]]) && this.a == that.a && this.b == that.b
    case _ => false
  }
  override def toString = "Vote(" + a.toString() + ", " + b.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (1) + a.hashCode) + b.hashCode
}
final case class Fwd[A](a: A) extends msg[A]
{
  override def equals(other: Any) = other match {
    case that:Fwd[A] => (that.isInstanceOf[Fwd[A]]) && this.a == that.a
    case _ => false
  }
  override def toString = "Fwd(" + a.toString() + ")"
  override def hashCode : Int = a.hashCode
}

def equal_msg[A : MicroCheckerLib.equal](x0: msg[A], x1: msg[A]): Boolean =
  (x0, x1) match {
  case (Vote(x51, x52), Fwd(x6)) => false
  case (Fwd(x6), Vote(x51, x52)) => false
  case (Phase2b(x41, x42, x43, x44), Fwd(x6)) => false
  case (Fwd(x6), Phase2b(x41, x42, x43, x44)) => false
  case (Phase2b(x41, x42, x43, x44), Vote(x51, x52)) => false
  case (Vote(x51, x52), Phase2b(x41, x42, x43, x44)) => false
  case (Phase2a(x31, x32, x33, x34), Fwd(x6)) => false
  case (Fwd(x6), Phase2a(x31, x32, x33, x34)) => false
  case (Phase2a(x31, x32, x33, x34), Vote(x51, x52)) => false
  case (Vote(x51, x52), Phase2a(x31, x32, x33, x34)) => false
  case (Phase2a(x31, x32, x33, x34), Phase2b(x41, x42, x43, x44)) => false
  case (Phase2b(x41, x42, x43, x44), Phase2a(x31, x32, x33, x34)) => false
  case (Phase1b(x21, x22, x23), Fwd(x6)) => false
  case (Fwd(x6), Phase1b(x21, x22, x23)) => false
  case (Phase1b(x21, x22, x23), Vote(x51, x52)) => false
  case (Vote(x51, x52), Phase1b(x21, x22, x23)) => false
  case (Phase1b(x21, x22, x23), Phase2b(x41, x42, x43, x44)) => false
  case (Phase2b(x41, x42, x43, x44), Phase1b(x21, x22, x23)) => false
  case (Phase1b(x21, x22, x23), Phase2a(x31, x32, x33, x34)) => false
  case (Phase2a(x31, x32, x33, x34), Phase1b(x21, x22, x23)) => false
  case (Phase1a(x11, x12), Fwd(x6)) => false
  case (Fwd(x6), Phase1a(x11, x12)) => false
  case (Phase1a(x11, x12), Vote(x51, x52)) => false
  case (Vote(x51, x52), Phase1a(x11, x12)) => false
  case (Phase1a(x11, x12), Phase2b(x41, x42, x43, x44)) => false
  case (Phase2b(x41, x42, x43, x44), Phase1a(x11, x12)) => false
  case (Phase1a(x11, x12), Phase2a(x31, x32, x33, x34)) => false
  case (Phase2a(x31, x32, x33, x34), Phase1a(x11, x12)) => false
  case (Phase1a(x11, x12), Phase1b(x21, x22, x23)) => false
  case (Phase1b(x21, x22, x23), Phase1a(x11, x12)) => false
  case (Fwd(x6), Fwd(y6)) => MicroCheckerLib.eq[A](x6, y6)
  case (Vote(x51, x52), Vote(y51, y52)) =>
    MicroCheckerLib.equal_nata(x51, y51) && equal_cmda[A](x52, y52)
  case (Phase2b(x41, x42, x43, x44), Phase2b(y41, y42, y43, y44)) =>
    MicroCheckerLib.equal_nata(x41, y41) &&
      (MicroCheckerLib.equal_nata(x42, y42) &&
        (MicroCheckerLib.equal_nata(x43, y43) && equal_cmda[A](x44, y44)))
  case (Phase2a(x31, x32, x33, x34), Phase2a(y31, y32, y33, y34)) =>
    MicroCheckerLib.equal_nata(x31, y31) &&
      (MicroCheckerLib.equal_nata(x32, y32) &&
        (equal_cmda[A](x33, y33) && MicroCheckerLib.equal_nata(x34, y34)))
  case (Phase1b(x21, x22, x23), Phase1b(y21, y22, y23)) =>
    MicroCheckerLib.equal_finfuna[MicroCheckerLib.nat,
                                   Option[(cmd[A],
    MicroCheckerLib.nat)]](x21, y21) &&
      (MicroCheckerLib.equal_nata(x22, y22) &&
        MicroCheckerLib.equal_nata(x23, y23))
  case (Phase1a(x11, x12), Phase1a(y11, y12)) =>
    MicroCheckerLib.equal_nata(x11, y11) && MicroCheckerLib.equal_nata(x12, y12)
}

abstract sealed class packet[A]
final case class
  Packet[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat, c: msg[A])
  extends packet[A]
{
  override def equals(other: Any) = other match {
    case that:Packet[A] => (that.isInstanceOf[Packet[A]]) && this.a == that.a && this.b == that.b && this.c == that.c
    case _ => false
  }
  override def toString = "Packet(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode
}

def equal_packeta[A : MicroCheckerLib.equal](x0: packet[A], x1: packet[A]):
      Boolean
  =
  (x0, x1) match {
  case (Packet(x1, x2, x3), Packet(y1, y2, y3)) =>
    MicroCheckerLib.equal_nata(x1, y1) &&
      (MicroCheckerLib.equal_nata(x2, y2) && equal_msg[A](x3, y3))
}

implicit def
  equal_packet[A : MicroCheckerLib.equal]: MicroCheckerLib.equal[packet[A]] =
  new MicroCheckerLib.equal[packet[A]] {
  val `MicroCheckerLib.equal` = (a: packet[A], b: packet[A]) =>
    equal_packeta[A](a, b)
}

abstract sealed class acc_state_ext[A, B]
final case class
  acc_state_exta[A, B](a: MicroCheckerLib.nat, b: Boolean,
                        c: MicroCheckerLib.fset[MicroCheckerLib.nat],
                        d: Option[MicroCheckerLib.nat],
                        e: MicroCheckerLib.finfun[MicroCheckerLib.nat,
           Option[cmd[A]]],
                        f: MicroCheckerLib.finfun[MicroCheckerLib.nat,
           Option[cmd[A]]],
                        g: MicroCheckerLib.finfun[MicroCheckerLib.nat,
           Option[MicroCheckerLib.nat]],
                        h: MicroCheckerLib.finfun[MicroCheckerLib.nat,
           MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                   List[(MicroCheckerLib.nat,
  Option[(cmd[A], MicroCheckerLib.nat)])]]],
                        i: MicroCheckerLib.finfun[MicroCheckerLib.nat,
           MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                   List[MicroCheckerLib.nat]]],
                        j: MicroCheckerLib.nat, k: Option[MicroCheckerLib.nat],
                        l: MicroCheckerLib.finfun[MicroCheckerLib.nat, Boolean],
                        m: MicroCheckerLib.finfun[MicroCheckerLib.nat,
           Option[cmd[A]]],
                        n: MicroCheckerLib.nat, o: MicroCheckerLib.nat,
                        p: List[MicroCheckerLib.nat], q: B)
  extends acc_state_ext[A, B]
{
  override def equals(other: Any) = other match {
    case that:acc_state_exta[A,B] => (that.isInstanceOf[acc_state_exta[A,B]]) && this.a == that.a && this.b == that.b && this.c == that.c && this.d == that.d && this.e == that.e && this.f == that.f && this.g == that.g && this.h == that.h && this.i == that.i && this.j == that.j && this.k == that.k && this.l == that.l && this.m == that.m && this.n == that.n && this.o == that.o && this.p == that.p
    case _ => false
  }
  override def toString = "id: " + a.toString() + "    leader: " + b.toString() + ",    acceptors: " + c.toString() + ",    ballot: " + d.toString() + ",    decided: " + e.toString() + ",    vote: " + f.toString() + ",    last_ballot: " + g.toString() + "\n" + "    onebs: " + h.toString() + ",    twobs: " + i.toString() + "\n" + "    next_inst: " + j.toString() + ",    last_decision: " + k.toString() + ",    working_instances: " + l.toString() + ",    commit_buffer: " + m.toString() + ",    last_commited: " + n.toString() + ",    snapshot_reference: " + o.toString() + ",    snapshot_proposal: " + p.toString() + "\n"
  override def hashCode : Int =   41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode) + d.hashCode) + e.hashCode) + f.hashCode) + g.hashCode) + h.hashCode) + i.hashCode) + j.hashCode) + k.hashCode) + l.hashCode) + m.hashCode) + n.hashCode) + o.hashCode) + p.hashCode
}

def equal_acc_state_exta[A : MicroCheckerLib.equal,
                          B : MicroCheckerLib.equal](x0: acc_state_ext[A, B],
              x1: acc_state_ext[A, B]):
      Boolean
  =
  (x0, x1) match {
  case (acc_state_exta(ida, leadera, acceptorsa, ballota, decideda, votea,
                        last_ballota, onebsa, twobsa, next_insta,
                        last_decisiona, working_instancesa, commit_buffera,
                        last_committeda, snapshot_referencea,
                        snapshot_proposala, morea),
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => MicroCheckerLib.equal_nata(ida, id) &&
         (MicroCheckerLib.equal_boola(leadera, leader) &&
           (MicroCheckerLib.equal_fset[MicroCheckerLib.nat](acceptorsa,
                     acceptors) &&
             (Optiona.equal_optiona[MicroCheckerLib.nat](ballota, ballot) &&
               (MicroCheckerLib.equal_finfuna[MicroCheckerLib.nat,
       Option[cmd[A]]](decideda, decided) &&
                 (MicroCheckerLib.equal_finfuna[MicroCheckerLib.nat,
         Option[cmd[A]]](votea, vote) &&
                   (MicroCheckerLib.equal_finfuna[MicroCheckerLib.nat,
           Option[MicroCheckerLib.nat]](last_ballota, last_ballot) &&
                     (MicroCheckerLib.equal_finfuna[MicroCheckerLib.nat,
             MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                     List[(MicroCheckerLib.nat,
    Option[(cmd[A], MicroCheckerLib.nat)])]]](onebsa, onebs) &&
                       (MicroCheckerLib.equal_finfuna[MicroCheckerLib.nat,
               MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                       List[MicroCheckerLib.nat]]](twobsa,
                            twobs) &&
                         (MicroCheckerLib.equal_nata(next_insta, next_inst) &&
                           (Optiona.equal_optiona[MicroCheckerLib.nat](last_decisiona,
                                last_decision) &&
                             (MicroCheckerLib.equal_finfuna[MicroCheckerLib.nat,
                     Boolean](working_instancesa, working_instances) &&
                               (MicroCheckerLib.equal_finfuna[MicroCheckerLib.nat,
                       Option[cmd[A]]](commit_buffera, commit_buffer) &&
                                 (MicroCheckerLib.equal_nata(last_committeda,
                      last_committed) &&
                                   (MicroCheckerLib.equal_nata(snapshot_referencea,
                        snapshot_reference) &&
                                     (MicroCheckerLib.equal_lista[MicroCheckerLib.nat](snapshot_proposala,
        snapshot_proposal) &&
                                       MicroCheckerLib.eq[B](morea,
                      more))))))))))))))))
}

implicit def
  equal_acc_state_ext[A : MicroCheckerLib.equal, B : MicroCheckerLib.equal]:
    MicroCheckerLib.equal[acc_state_ext[A, B]]
  = new MicroCheckerLib.equal[acc_state_ext[A, B]] {
  val `MicroCheckerLib.equal` = (a: acc_state_ext[A, B], b: acc_state_ext[A, B])
    => equal_acc_state_exta[A, B](a, b)
}

abstract sealed class mp_action[A]
final case class
  Receive_fwd[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat, c: A)
  extends mp_action[A]
{
  override def equals(other: Any) = other match {
    case that:Receive_fwd[A] => (that.isInstanceOf[Receive_fwd[A]]) && this.a == that.a && this.b == that.b && this.c == that.c
    case _ => false
  }
  override def toString = "Receive_fwd(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode
}
final case class Propose[A](a: MicroCheckerLib.nat, b: cmd[A]) extends
  mp_action[A]
{
  override def equals(other: Any) = other match {
    case that:Propose[A] => (that.isInstanceOf[Propose[A]]) && this.a == that.a && this.b == that.b
    case _ => false
  }
  override def toString = "Propose(" + a.toString() + ", " + b.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (1) + a.hashCode) + b.hashCode
}
final case class Send_1as[A](a: MicroCheckerLib.nat) extends mp_action[A]
{
  override def equals(other: Any) = other match {
    case that:Send_1as[A] => (that.isInstanceOf[Send_1as[A]]) && this.a == that.a
    case _ => false
  }
  override def toString = "Send_1as(" + a.toString() + ")"
  override def hashCode : Int = a.hashCode
}
final case class
  Receive_1a_send_1b[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat,
                         c: MicroCheckerLib.nat)
  extends mp_action[A]
{
  override def equals(other: Any) = other match {
    case that:Receive_1a_send_1b[A] => (that.isInstanceOf[Receive_1a_send_1b[A]]) && this.a == that.a && this.b == that.b && this.c == that.c
    case _ => false
  }
  override def toString = "Receive_1a_send_1b(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode
}
final case class
  Receive_1b[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat,
                 c: MicroCheckerLib.finfun[MicroCheckerLib.nat,
    Option[(cmd[A], MicroCheckerLib.nat)]],
                 d: MicroCheckerLib.nat)
  extends mp_action[A]
{
  override def equals(other: Any) = other match {
    case that:Receive_1b[A] => (that.isInstanceOf[Receive_1b[A]]) && this.a == that.a && this.b == that.b && this.c == that.c && this.d == that.d
    case _ => false
  }
  override def toString = "Receive_1b(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ", " + d.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode) + d.hashCode
}
final case class
  Receive_2a_send_2b[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat,
                         c: MicroCheckerLib.nat, d: MicroCheckerLib.nat,
                         e: cmd[A])
  extends mp_action[A]
{
  override def equals(other: Any) = other match {
    case that:Receive_2a_send_2b[A] => (that.isInstanceOf[Receive_2a_send_2b[A]]) && this.a == that.a && this.b == that.b && this.c == that.c && this.d == that.d && this.e == that.e
    case _ => false
  }
  override def toString = "Receive_2a_send_2b(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ", " + d.toString() + ", " + e.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode) + d.hashCode) + e.hashCode
}
final case class
  Receive_2b[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat,
                 c: MicroCheckerLib.nat, d: MicroCheckerLib.nat, e: cmd[A])
  extends mp_action[A]
{
  override def equals(other: Any) = other match {
    case that:Receive_2b[A] => (that.isInstanceOf[Receive_2b[A]]) && this.a == that.a && this.b == that.b && this.c == that.c && this.d == that.d && this.e == that.e
    case _ => false
  }
  override def toString = "Receive_2b(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ", " + d.toString() + ", " + e.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode) + d.hashCode) + e.hashCode
}
final case class
  Learn[A](a: MicroCheckerLib.nat, b: MicroCheckerLib.nat, c: cmd[A])
  extends mp_action[A]
{
  override def equals(other: Any) = other match {
    case that:Learn[A] => (that.isInstanceOf[Learn[A]]) && this.a == that.a && this.b == that.b && this.c == that.c
    case _ => false
  }
  override def toString = "Learn(" + a.toString() + ", " + b.toString() + ", " + c.toString() + ")"
  override def hashCode : Int =   41 * (  41 * (  41 * (1) + a.hashCode) + b.hashCode) + c.hashCode
}

abstract sealed class mp_state_ext[A, B]
final case class
  mp_state_exta[A, B](a: MicroCheckerLib.finfun[MicroCheckerLib.nat,
         acc_state_ext[A, Unit]],
                       b: MicroCheckerLib.fset[packet[A]], c: B)
  extends mp_state_ext[A, B]
{
  override def equals(other: Any) = other match {
    case that:mp_state_exta[A,B] => (that.isInstanceOf[mp_state_exta[A,B]]) && this.a == that.a && this.b == that.b
    case _ => false
  }
  override def toString = "node_states:\n" + a.toString() + "\n" + "network: " + b.toString()
  override def hashCode : Int =   41 * (  41 * (1) + a.hashCode) + b.hashCode
}

def acceptors[A, B](x0: acc_state_ext[A, B]):
      MicroCheckerLib.fset[MicroCheckerLib.nat]
  =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => acceptors
}

def nr[A, B](s: acc_state_ext[A, B]): MicroCheckerLib.nat =
  MicroCheckerLib.fcard[MicroCheckerLib.nat](acceptors[A, B](s))

def accs(n: MicroCheckerLib.nat): MicroCheckerLib.fset[MicroCheckerLib.nat] =
  (if (MicroCheckerLib.equal_nata(n, MicroCheckerLib.zero_nat))
    MicroCheckerLib.bot_fset[MicroCheckerLib.nat]
    else MicroCheckerLib.sup_fset[MicroCheckerLib.nat](accs(MicroCheckerLib.minus_nat(n,
       MicroCheckerLib.one_nat)),
                MicroCheckerLib.finsert[MicroCheckerLib.nat](MicroCheckerLib.minus_nat(n,
        MicroCheckerLib.one_nat),
                      MicroCheckerLib.bot_fset[MicroCheckerLib.nat])))

def working_instances_update[A, B](working_instancesa:
                                     (MicroCheckerLib.finfun[MicroCheckerLib.nat,
                      Boolean]) =>
                                       MicroCheckerLib.finfun[MicroCheckerLib.nat,
                       Boolean],
                                    x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (working_instancesa, x1) match {
  case (working_instancesa,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballot, decided, vote,
                             last_ballot, onebs, twobs, next_inst,
                             last_decision,
                             working_instancesa(working_instances),
                             commit_buffer, last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def working_instances[A, B](x0: acc_state_ext[A, B]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat, Boolean]
  =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => working_instances
}

def next_inst_update[A, B](next_insta:
                             MicroCheckerLib.nat => MicroCheckerLib.nat,
                            x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (next_insta, x1) match {
  case (next_insta,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballot, decided, vote,
                             last_ballot, onebs, twobs, next_insta(next_inst),
                             last_decision, working_instances, commit_buffer,
                             last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def twobs_update[A, B](twobsa:
                         (MicroCheckerLib.finfun[MicroCheckerLib.nat,
          MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                  List[MicroCheckerLib.nat]]]) =>
                           MicroCheckerLib.finfun[MicroCheckerLib.nat,
           MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                   List[MicroCheckerLib.nat]]],
                        x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (twobsa, x1) match {
  case (twobsa,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballot, decided, vote,
                             last_ballot, onebs, twobsa(twobs), next_inst,
                             last_decision, working_instances, commit_buffer,
                             last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def next_inst[A, B](x0: acc_state_ext[A, B]): MicroCheckerLib.nat = x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => next_inst
}

def ballot[A, B](x0: acc_state_ext[A, B]): Option[MicroCheckerLib.nat] = x0
  match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => ballot
}

def twobs[A, B](x0: acc_state_ext[A, B]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat,
                              MicroCheckerLib.finfun[MicroCheckerLib.nat,
              List[MicroCheckerLib.nat]]]
  =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => twobs
}

def id[A, B](x0: acc_state_ext[A, B]): MicroCheckerLib.nat = x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => id
}

def send_all[A, B,
              C](s: acc_state_ext[A, B], a: MicroCheckerLib.nat, m: msg[C]):
      MicroCheckerLib.fset[packet[C]]
  =
  MicroCheckerLib.fimage[MicroCheckerLib.nat,
                          packet[C]](((a2: MicroCheckerLib.nat) =>
                                       Packet[C](id[A, B](s), a2, m)),
                                      MicroCheckerLib.minus_fset[MicroCheckerLib.nat](acceptors[A,
                 B](s),
       MicroCheckerLib.finsert[MicroCheckerLib.nat](a,
             MicroCheckerLib.bot_fset[MicroCheckerLib.nat])))

def do_2a[A, B, C](s: acc_state_ext[A, B], v: cmd[C]):
      (acc_state_ext[A, B], MicroCheckerLib.fset[packet[C]])
  =
  {
    val a: MicroCheckerLib.nat = id[A, B](s)
    val inst: MicroCheckerLib.nat = next_inst[A, B](s)
    val b: MicroCheckerLib.nat =
      Optiona.the[MicroCheckerLib.nat](ballot[A, B](s))
    val msg: msg[C] = Phase2a[C](inst, b, v, a)
    val new_state: acc_state_ext[A, B] =
      working_instances_update[A, B](((_:
 MicroCheckerLib.finfun[MicroCheckerLib.nat, Boolean])
=>
                                       MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
                              Boolean](working_instances[A, B](s), inst, true)),
                                      twobs_update[A,
            B](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat,
    MicroCheckerLib.finfun[MicroCheckerLib.nat, List[MicroCheckerLib.nat]]])
                  =>
                 MicroCheckerLib.finfun_update_code[MicroCheckerLib.nat,
             MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                     List[MicroCheckerLib.nat]]](twobs[A, B](s),
                          inst,
                          MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
                 List[MicroCheckerLib.nat]](MicroCheckerLib.finfun_const[List[MicroCheckerLib.nat],
                                  MicroCheckerLib.nat](Nil),
     b, List(a)))),
                next_inst_update[A, B](((_: MicroCheckerLib.nat) =>
 MicroCheckerLib.plus_nat(inst, MicroCheckerLib.one_nat)),
s)));
    (new_state, send_all[A, B, C](s, a, msg))
  }

def decided[A, B](x0: acc_state_ext[A, B]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat, Option[cmd[A]]]
  =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => decided
}

def learn[A : MicroCheckerLib.equal](i: MicroCheckerLib.nat, v: A,
                                      s: acc_state_ext[A, Unit]):
      Option[(acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])]
  =
  (MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                                 Option[cmd[A]]](decided[A, Unit](s), i)
     match {
     case None => None
     case Some(Cmd(c)) =>
       (if (MicroCheckerLib.eq[A](v, c))
         Some[(acc_state_ext[A, Unit],
                MicroCheckerLib.fset[packet[A]])]((s,
            MicroCheckerLib.bot_fset[packet[A]]))
         else None)
     case Some(NoOp()) => None
   })

def getOwnedBallot(a: MicroCheckerLib.nat, b: MicroCheckerLib.nat,
                    n: MicroCheckerLib.nat):
      MicroCheckerLib.nat
  =
  (if (MicroCheckerLib.equal_nata(b, MicroCheckerLib.zero_nat))
    MicroCheckerLib.zero_nat
    else (if (MicroCheckerLib.equal_nata(MicroCheckerLib.mod_nat(MicroCheckerLib.Suc(MicroCheckerLib.minus_nat(b,
                                MicroCheckerLib.one_nat)),
                          n),
  a))
           MicroCheckerLib.Suc(MicroCheckerLib.minus_nat(b,
                  MicroCheckerLib.one_nat))
           else getOwnedBallot(a, MicroCheckerLib.minus_nat(b,
                     MicroCheckerLib.one_nat),
                                n)))

def suc_bal(a: MicroCheckerLib.nat, b: MicroCheckerLib.nat,
             n: MicroCheckerLib.nat):
      MicroCheckerLib.nat
  =
  getOwnedBallot(a, MicroCheckerLib.plus_nat(b, n), n)

def nx_bal(a: MicroCheckerLib.nat, x1: Option[MicroCheckerLib.nat],
            n: MicroCheckerLib.nat):
      MicroCheckerLib.nat
  =
  (a, x1, n) match {
  case (a, None, n) => suc_bal(a, MicroCheckerLib.zero_nat, n)
  case (a, Some(b), n) => suc_bal(a, b, n)
}

def leader[A, B](x0: acc_state_ext[A, B]): Boolean = x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => leader
}

def leader_of_bal[A, B](s: acc_state_ext[A, B], b: Option[MicroCheckerLib.nat]):
      MicroCheckerLib.nat
  =
  (b match {
     case None => MicroCheckerLib.zero_nat
     case Some(ba) => MicroCheckerLib.mod_nat(ba, nr[A, B](s))
   })

def propose[A : MicroCheckerLib.equal](v: A, s: acc_state_ext[A, Unit]):
      (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
  =
  {
    val a: MicroCheckerLib.nat = id[A, Unit](s);
    (if (MicroCheckerLib.equal_nata(leader_of_bal[A,
           Unit](s, ballot[A, Unit](s)),
                                     a) &&
           leader[A, Unit](s))
      do_2a[A, Unit, A](s, Cmd[A](v))
      else (if (MicroCheckerLib.equal_nata(leader_of_bal[A,
                  Unit](s, ballot[A, Unit](s)),
    a))
             (s, MicroCheckerLib.bot_fset[packet[A]])
             else (s, MicroCheckerLib.finsert[packet[A]](Packet[A](a,
                            leader_of_bal[A, Unit](s, ballot[A, Unit](s)),
                            Fwd[A](v)),
                  MicroCheckerLib.bot_fset[packet[A]]))))
  }

def node_states[A, B](x0: mp_state_ext[A, B]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat, acc_state_ext[A, Unit]]
  =
  x0 match {
  case mp_state_exta(node_states, network, more) => node_states
}

def safe_at[A : MicroCheckerLib.equal](s: mp_state_ext[A, Unit],
i: MicroCheckerLib.nat):
      Boolean
  =
  MicroCheckerLib.Ball[MicroCheckerLib.nat](MicroCheckerLib.insert[MicroCheckerLib.nat](MicroCheckerLib.zero_nat,
         MicroCheckerLib.insert[MicroCheckerLib.nat](MicroCheckerLib.one_nat,
              MicroCheckerLib.insert[MicroCheckerLib.nat](MicroCheckerLib.nat_of_integer(BigInt(2)),
                   MicroCheckerLib.bot_set[MicroCheckerLib.nat]))),
     ((acc_1: MicroCheckerLib.nat) =>
       MicroCheckerLib.Ball[MicroCheckerLib.nat](MicroCheckerLib.insert[MicroCheckerLib.nat](MicroCheckerLib.zero_nat,
              MicroCheckerLib.insert[MicroCheckerLib.nat](MicroCheckerLib.one_nat,
                   MicroCheckerLib.insert[MicroCheckerLib.nat](MicroCheckerLib.nat_of_integer(BigInt(2)),
                        MicroCheckerLib.bot_set[MicroCheckerLib.nat]))),
          ((acc_2: MicroCheckerLib.nat) =>
            (if (! (MicroCheckerLib.equal_nata(acc_1, acc_2)))
              (MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
     Option[cmd[A]]](decided[A, Unit](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                            acc_state_ext[A,
   Unit]](node_states[A, Unit](s), acc_1)),
                      i)
                 match {
                 case None => true
                 case Some(cm) =>
                   (MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
          Option[cmd[A]]](decided[A, Unit](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                                 acc_state_ext[A,
        Unit]](node_states[A, Unit](s), acc_2)),
                           i)
                      match {
                      case None => true
                      case Some(a) => equal_cmda[A](cm, a)
                    })
               })
              else true)))))

def ballot_update[A, B](ballota:
                          Option[MicroCheckerLib.nat] =>
                            Option[MicroCheckerLib.nat],
                         x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (ballota, x1) match {
  case (ballota,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballota(ballot), decided,
                             vote, last_ballot, onebs, twobs, next_inst,
                             last_decision, working_instances, commit_buffer,
                             last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def send_1a[A](s: acc_state_ext[A, Unit]):
      (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
  =
  {
    val a: MicroCheckerLib.nat = id[A, Unit](s)
    val b: MicroCheckerLib.nat = nx_bal(a, ballot[A, Unit](s), nr[A, Unit](s))
    val msg_1a: msg[A] = Phase1a[A](a, b);
    (ballot_update[A, Unit](((_: Option[MicroCheckerLib.nat]) =>
                              Some[MicroCheckerLib.nat](b)),
                             s),
      MicroCheckerLib.fimage[MicroCheckerLib.nat,
                              packet[A]](((a2: MicroCheckerLib.nat) =>
   Packet[A](a, a2, msg_1a)),
  acceptors[A, Unit](s)))
  }

def init_acc_state[A](n: MicroCheckerLib.nat, a: MicroCheckerLib.nat):
      acc_state_ext[A, Unit]
  =
  acc_state_exta[A, Unit](a, false, accs(n), None,
                           MicroCheckerLib.finfun_const[Option[cmd[A]],
                 MicroCheckerLib.nat](None),
                           MicroCheckerLib.finfun_const[Option[cmd[A]],
                 MicroCheckerLib.nat](None),
                           MicroCheckerLib.finfun_const[Option[MicroCheckerLib.nat],
                 MicroCheckerLib.nat](None),
                           MicroCheckerLib.finfun_const[MicroCheckerLib.finfun[MicroCheckerLib.nat,
List[(MicroCheckerLib.nat, Option[(cmd[A], MicroCheckerLib.nat)])]],
                 MicroCheckerLib.nat](MicroCheckerLib.finfun_const[List[(MicroCheckerLib.nat,
                                  Option[(cmd[A], MicroCheckerLib.nat)])],
                            MicroCheckerLib.nat](Nil)),
                           MicroCheckerLib.finfun_const[MicroCheckerLib.finfun[MicroCheckerLib.nat,
List[MicroCheckerLib.nat]],
                 MicroCheckerLib.nat](MicroCheckerLib.finfun_const[List[MicroCheckerLib.nat],
                            MicroCheckerLib.nat](Nil)),
                           MicroCheckerLib.one_nat, None,
                           MicroCheckerLib.finfun_const[Boolean,
                 MicroCheckerLib.nat](false),
                           MicroCheckerLib.finfun_const[Option[cmd[A]],
                 MicroCheckerLib.nat](None),
                           MicroCheckerLib.zero_nat, MicroCheckerLib.zero_nat,
                           Nil, ())

def init_nodes_state[A : MicroCheckerLib.equal](i: MicroCheckerLib.nat,
         n: MicroCheckerLib.nat):
      MicroCheckerLib.finfun[MicroCheckerLib.nat, acc_state_ext[A, Unit]]
  =
  (if (MicroCheckerLib.equal_nata(i, MicroCheckerLib.zero_nat))
    MicroCheckerLib.finfun_const[acc_state_ext[A, Unit],
                                  MicroCheckerLib.nat](init_acc_state[A](n,
                                  MicroCheckerLib.zero_nat))
    else (if (MicroCheckerLib.less_nat(n,
MicroCheckerLib.Suc(MicroCheckerLib.minus_nat(i, MicroCheckerLib.one_nat))))
           sys.error("undefined")
           else MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
       acc_state_ext[A, Unit]](init_nodes_state[A](MicroCheckerLib.minus_nat(i,
                                      MicroCheckerLib.one_nat),
            n),
                                MicroCheckerLib.minus_nat(i,
                   MicroCheckerLib.one_nat),
                                init_acc_state[A](n,
           MicroCheckerLib.minus_nat(i, MicroCheckerLib.one_nat)))))

def mp_start[A : MicroCheckerLib.equal]: mp_state_ext[A, Unit] =
  mp_state_exta[A, Unit](init_nodes_state[A](MicroCheckerLib.nat_of_integer(BigInt(3)),
      MicroCheckerLib.nat_of_integer(BigInt(3))),
                          MicroCheckerLib.bot_fset[packet[A]], ())

def is_leader[A, B](s: acc_state_ext[A, B]): Boolean = leader[A, B](s)

def new_twobs[A, B](s: acc_state_ext[A, B], i: MicroCheckerLib.nat,
                     b: MicroCheckerLib.nat, a: MicroCheckerLib.nat):
      List[MicroCheckerLib.nat]
  =
  a :: MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                                     List[MicroCheckerLib.nat]](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
              MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                      List[MicroCheckerLib.nat]]](twobs[A,
                                 B](s),
                           i),
                         b)

def vote_update[A, B](votea:
                        (MicroCheckerLib.finfun[MicroCheckerLib.nat,
         Option[cmd[A]]]) =>
                          MicroCheckerLib.finfun[MicroCheckerLib.nat,
          Option[cmd[A]]],
                       x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (votea, x1) match {
  case (votea,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballot, decided, votea(vote),
                             last_ballot, onebs, twobs, next_inst,
                             last_decision, working_instances, commit_buffer,
                             last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def last_decision_update[A, B](last_decisiona:
                                 Option[MicroCheckerLib.nat] =>
                                   Option[MicroCheckerLib.nat],
                                x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (last_decisiona, x1) match {
  case (last_decisiona,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballot, decided, vote,
                             last_ballot, onebs, twobs, next_inst,
                             last_decisiona(last_decision), working_instances,
                             commit_buffer, last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def commit_buffer_update[A, B](commit_buffera:
                                 (MicroCheckerLib.finfun[MicroCheckerLib.nat,
                  Option[cmd[A]]]) =>
                                   MicroCheckerLib.finfun[MicroCheckerLib.nat,
                   Option[cmd[A]]],
                                x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (commit_buffera, x1) match {
  case (commit_buffera,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballot, decided, vote,
                             last_ballot, onebs, twobs, next_inst,
                             last_decision, working_instances,
                             commit_buffera(commit_buffer), last_committed,
                             snapshot_reference, snapshot_proposal, more)
}

def decided_update[A, B](decideda:
                           (MicroCheckerLib.finfun[MicroCheckerLib.nat,
            Option[cmd[A]]]) =>
                             MicroCheckerLib.finfun[MicroCheckerLib.nat,
             Option[cmd[A]]],
                          x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (decideda, x1) match {
  case (decideda,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballot, decideda(decided),
                             vote, last_ballot, onebs, twobs, next_inst,
                             last_decision, working_instances, commit_buffer,
                             last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def commit_buffer[A, B](x0: acc_state_ext[A, B]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat, Option[cmd[A]]]
  =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => commit_buffer
}

def update_decided[A : MicroCheckerLib.equal,
                    B](s: acc_state_ext[A, B], i: MicroCheckerLib.nat,
                        v: cmd[A]):
      acc_state_ext[A, B]
  =
  commit_buffer_update[A, B](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat,
                  Option[cmd[A]]])
                                =>
                               MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
                      Option[cmd[A]]](commit_buffer[A, B](s), i,
                                       Some[cmd[A]](v))),
                              last_decision_update[A,
            B](((_: Option[MicroCheckerLib.nat]) =>
                 Some[MicroCheckerLib.nat](i)),
                decided_update[A, B](((_:
 MicroCheckerLib.finfun[MicroCheckerLib.nat, Option[cmd[A]]])
=>
                                       MicroCheckerLib.finfun_update_code[MicroCheckerLib.nat,
                                   Option[cmd[A]]](decided[A, B](s), i,
            Some[cmd[A]](v))),
                                      s)))

def vote[A, B](x0: acc_state_ext[A, B]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat, Option[cmd[A]]]
  =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => vote
}

def receive_2_first[A : MicroCheckerLib.equal](i: MicroCheckerLib.nat,
        b: MicroCheckerLib.nat, v: cmd[A], l: MicroCheckerLib.nat,
        s: acc_state_ext[A, Unit]):
      acc_state_ext[A, Unit]
  =
  {
    val a: MicroCheckerLib.nat = id[A, Unit](s)
    val bal: Option[MicroCheckerLib.nat] = ballot[A, Unit](s)
    val s2: acc_state_ext[A, Unit] =
      working_instances_update[A, Unit](((_:
    MicroCheckerLib.finfun[MicroCheckerLib.nat, Boolean])
   =>
  MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
                                 Boolean](working_instances[A, Unit](s), i,
   true)),
 next_inst_update[A, Unit](((_: MicroCheckerLib.nat) =>
                             MicroCheckerLib.plus_nat(i,
               MicroCheckerLib.one_nat)),
                            twobs_update[A,
  Unit](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                     MicroCheckerLib.finfun[MicroCheckerLib.nat,
                     List[MicroCheckerLib.nat]]])
           =>
          MicroCheckerLib.finfun_update_code[MicroCheckerLib.nat,
      MicroCheckerLib.finfun[MicroCheckerLib.nat,
                              List[MicroCheckerLib.nat]]](twobs[A, Unit](s), i,
                   MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
          List[MicroCheckerLib.nat]](MicroCheckerLib.finfun_const[List[MicroCheckerLib.nat],
                           MicroCheckerLib.nat](Nil),
                                      Optiona.the[MicroCheckerLib.nat](bal),
                                      List(a, l)))),
         vote_update[A, Unit](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat,
                   Option[cmd[A]]])
                                 =>
                                MicroCheckerLib.finfun_update_code[MicroCheckerLib.nat,
                            Option[cmd[A]]](vote[A, Unit](s), i,
     Some[cmd[A]](v))),
                               s))));
    (if (MicroCheckerLib.less_nat(MicroCheckerLib.nat_of_integer(BigInt(3)),
                                   nr[A, Unit](s)))
      s2 else (if (MicroCheckerLib.equal_nata(MicroCheckerLib.nat_of_integer(BigInt(3)),
       nr[A, Unit](s)))
                update_decided[A, Unit](s2, i, v)
                else {
                       val s3: acc_state_ext[A, Unit] =
                         working_instances_update[A,
           Unit](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat, Boolean]) =>
                   MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
          Boolean](working_instances[A, Unit](s), i, false)),
                  s2);
                       update_decided[A, Unit](s3, i, v)
                     }))
  }

def update_twobs[A, B](s: acc_state_ext[A, B], i: MicroCheckerLib.nat,
                        b: MicroCheckerLib.nat,
                        newa: List[MicroCheckerLib.nat]):
      acc_state_ext[A, B]
  =
  twobs_update[A, B](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat,
          MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                  List[MicroCheckerLib.nat]]])
                        =>
                       MicroCheckerLib.finfun_update_code[MicroCheckerLib.nat,
                   MicroCheckerLib.finfun[MicroCheckerLib.nat,
   List[MicroCheckerLib.nat]]](twobs[A, B](s), i,
                                MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
                       List[MicroCheckerLib.nat]](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
MicroCheckerLib.finfun[MicroCheckerLib.nat,
                        List[MicroCheckerLib.nat]]](twobs[A, B](s), i),
           b, newa))),
                      s)

def receive_2_addl[A : MicroCheckerLib.equal](i: MicroCheckerLib.nat,
       b: MicroCheckerLib.nat, v: cmd[A], l: MicroCheckerLib.nat,
       s: acc_state_ext[A, Unit]):
      acc_state_ext[A, Unit]
  =
  {
    id[A, Unit](s);
    ballot[A, Unit](s)
    val s2: acc_state_ext[A, Unit] =
      update_twobs[A, Unit](s, i, b, new_twobs[A, Unit](s, i, b, l))
    val votes: MicroCheckerLib.nat =
      MicroCheckerLib.size_list[MicroCheckerLib.nat].apply(MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
         List[MicroCheckerLib.nat]](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                          MicroCheckerLib.finfun[MicroCheckerLib.nat,
          List[MicroCheckerLib.nat]]](twobs[A, Unit](s2), i),
                                     b));
    (if (MicroCheckerLib.less_eq_nat(MicroCheckerLib.times_nat(MicroCheckerLib.nat_of_integer(BigInt(2)),
                        votes),
                                      nr[A, Unit](s)))
      s2 else (if (MicroCheckerLib.equal_nata(MicroCheckerLib.plus_nat(MicroCheckerLib.divide_nat(nr[A,
                      Unit](s),
                   MicroCheckerLib.nat_of_integer(BigInt(2))),
                                MicroCheckerLib.one_nat),
       votes))
                update_decided[A, Unit](s2, i, v)
                else (if (MicroCheckerLib.equal_nata(nr[A, Unit](s), votes))
                       {
                         val s3: acc_state_ext[A, Unit] =
                           working_instances_update[A,
             Unit](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat, Boolean]) =>
                     MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
            Boolean](working_instances[A, Unit](s), i, false)),
                    s2);
                         s3
                       }
                       else s2)))
  }

def receive_2[A : MicroCheckerLib.equal](i: MicroCheckerLib.nat,
  b: MicroCheckerLib.nat, v: cmd[A], l: MicroCheckerLib.nat,
  s: acc_state_ext[A, Unit]):
      acc_state_ext[A, Unit]
  =
  (if (MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                                     Boolean](working_instances[A, Unit](s), i))
    receive_2_addl[A](i, b, v, l, s) else receive_2_first[A](i, b, v, l, s))

def get_ballot[A, B](s: acc_state_ext[A, B]): Option[MicroCheckerLib.nat] =
  ballot[A, B](s)

def get_leader[A, B](s: acc_state_ext[A, B]): Option[MicroCheckerLib.nat] =
  (ballot[A, B](s) match {
     case None => None
     case Some(b) =>
       Some[MicroCheckerLib.nat](MicroCheckerLib.mod_nat(b, nr[A, B](s)))
   })

def node_states_update[A, B](node_statesa:
                               (MicroCheckerLib.finfun[MicroCheckerLib.nat,
                acc_state_ext[A, Unit]]) =>
                                 MicroCheckerLib.finfun[MicroCheckerLib.nat,
                 acc_state_ext[A, Unit]],
                              x1: mp_state_ext[A, B]):
      mp_state_ext[A, B]
  =
  (node_statesa, x1) match {
  case (node_statesa, mp_state_exta(node_states, network, more)) =>
    mp_state_exta[A, B](node_statesa(node_states), network, more)
}

def network_update[A, B](networka:
                           (MicroCheckerLib.fset[packet[A]]) =>
                             MicroCheckerLib.fset[packet[A]],
                          x1: mp_state_ext[A, B]):
      mp_state_ext[A, B]
  =
  (networka, x1) match {
  case (networka, mp_state_exta(node_states, network, more)) =>
    mp_state_exta[A, B](node_states, networka(network), more)
}

def network[A, B](x0: mp_state_ext[A, B]): MicroCheckerLib.fset[packet[A]] = x0
  match {
  case mp_state_exta(node_states, network, more) => network
}

def update_state[A : MicroCheckerLib.equal,
                  B](a: MicroCheckerLib.nat, a_s: acc_state_ext[A, Unit],
                      packets: MicroCheckerLib.fset[packet[A]],
                      s: mp_state_ext[A, B]):
      mp_state_ext[A, B]
  =
  network_update[A, B](((_: MicroCheckerLib.fset[packet[A]]) =>
                         MicroCheckerLib.sup_fset[packet[A]](network[A, B](s),
                      packets)),
                        node_states_update[A,
    B](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat, acc_state_ext[A, Unit]])
          =>
         MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
acc_state_ext[A, Unit]](node_states[A, B](s), a, a_s)),
        s))

def receive_fwd[A](v: A, s: acc_state_ext[A, Unit]):
      (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
  =
  {
    val a: MicroCheckerLib.nat = id[A, Unit](s);
    (if (MicroCheckerLib.equal_nata(leader_of_bal[A,
           Unit](s, ballot[A, Unit](s)),
                                     a) &&
           leader[A, Unit](s))
      do_2a[A, Unit, A](s, Cmd[A](v))
      else (s, MicroCheckerLib.bot_fset[packet[A]]))
  }

def receive_2b[A : MicroCheckerLib.equal](i: MicroCheckerLib.nat,
   b: MicroCheckerLib.nat, a2: MicroCheckerLib.nat, v: cmd[A],
   s: acc_state_ext[A, Unit]):
      (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
  =
  (receive_2[A](i, b, v, a2, s), MicroCheckerLib.bot_fset[packet[A]])

def receive_2a[A : MicroCheckerLib.equal](i: MicroCheckerLib.nat,
   b: MicroCheckerLib.nat, v: cmd[A], l: MicroCheckerLib.nat,
   s: acc_state_ext[A, Unit]):
      (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
  =
  (receive_2[A](i, b, v, l, s),
    send_all[A, Unit,
              A](s, id[A, Unit](s), Phase2b[A](i, b, id[A, Unit](s), v)))

def leader_update[A, B](leadera: Boolean => Boolean, x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (leadera, x1) match {
  case (leadera,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leadera(leader), acceptors, ballot, decided,
                             vote, last_ballot, onebs, twobs, next_inst,
                             last_decision, working_instances, commit_buffer,
                             last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def onebs[A, B](x0: acc_state_ext[A, B]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat,
                              MicroCheckerLib.finfun[MicroCheckerLib.nat,
              List[(MicroCheckerLib.nat,
                     Option[(cmd[A], MicroCheckerLib.nat)])]]]
  =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => onebs
}

def one_b_quorum_received[A, B](b: MicroCheckerLib.nat, s: acc_state_ext[A, B]):
      Boolean
  =
  {
    val at_b: MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                      List[(MicroCheckerLib.nat,
     Option[(cmd[A], MicroCheckerLib.nat)])]]
      = MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                                      MicroCheckerLib.finfun[MicroCheckerLib.nat,
                      List[(MicroCheckerLib.nat,
                             Option[(cmd[A],
                                      MicroCheckerLib.nat)])]]](onebs[A, B](s),
                         b)
    val at_b_i:
          List[(MicroCheckerLib.nat, Option[(cmd[A], MicroCheckerLib.nat)])]
      = MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                                      List[(MicroCheckerLib.nat,
     Option[(cmd[A], MicroCheckerLib.nat)])]](at_b, MicroCheckerLib.zero_nat);
    MicroCheckerLib.less_nat(nr[A, B](s),
                              MicroCheckerLib.times_nat(MicroCheckerLib.nat_of_integer(BigInt(2)),
                 MicroCheckerLib.size_list[(MicroCheckerLib.nat,
     Option[(cmd[A], MicroCheckerLib.nat)])].apply(at_b_i)))
  }

def highest_voted[A](m: MicroCheckerLib.finfun[MicroCheckerLib.nat,
        List[(MicroCheckerLib.nat, Option[(cmd[A], MicroCheckerLib.nat)])]]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat, Option[cmd[A]]]
  =
  {
    val votes:
          MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                  List[Option[(cmd[A], MicroCheckerLib.nat)]]]
      = MicroCheckerLib.finfun_comp[List[(MicroCheckerLib.nat,
   Option[(cmd[A], MicroCheckerLib.nat)])],
                                     List[Option[(cmd[A],
           MicroCheckerLib.nat)]],
                                     MicroCheckerLib.nat](((a:
                      List[(MicroCheckerLib.nat,
                             Option[(cmd[A], MicroCheckerLib.nat)])])
                     =>
                    MicroCheckerLib.map[(MicroCheckerLib.nat,
  Option[(cmd[A], MicroCheckerLib.nat)]),
 Option[(cmd[A],
          MicroCheckerLib.nat)]](((aa: (MicroCheckerLib.nat,
 Option[(cmd[A], MicroCheckerLib.nat)]))
                                    =>
                                   MicroCheckerLib.snd[MicroCheckerLib.nat,
                Option[(cmd[A], MicroCheckerLib.nat)]](aa)),
                                  a)),
                   m)
    val highest: (List[Option[(cmd[A], MicroCheckerLib.nat)]]) => Option[cmd[A]]
      = ((l: List[Option[(cmd[A], MicroCheckerLib.nat)]]) =>
          (if (MicroCheckerLib.nulla[Option[(cmd[A], MicroCheckerLib.nat)]](l))
            None
            else Optiona.bind[(cmd[A], MicroCheckerLib.nat),
                               cmd[A]](MicroCheckerLib.fold[Option[(cmd[A],
                             MicroCheckerLib.nat)],
                     Option[(cmd[A],
                              MicroCheckerLib.nat)]](((a:
                 Option[(cmd[A], MicroCheckerLib.nat)])
                =>
               (b: Option[(cmd[A], MicroCheckerLib.nat)]) =>
               MicroCheckerLib.max[Option[(cmd[A],
    MicroCheckerLib.nat)]](a, b)),
              l, MicroCheckerLib.nth[Option[(cmd[A],
      MicroCheckerLib.nat)]](l, MicroCheckerLib.zero_nat)),
((vb: (cmd[A], MicroCheckerLib.nat)) =>
  Some[cmd[A]](MicroCheckerLib.fst[cmd[A], MicroCheckerLib.nat](vb))))));
    MicroCheckerLib.finfun_comp[List[Option[(cmd[A], MicroCheckerLib.nat)]],
                                 Option[cmd[A]],
                                 MicroCheckerLib.nat](highest, votes)
  }

def onebs_update[A, B](onebsa:
                         (MicroCheckerLib.finfun[MicroCheckerLib.nat,
          MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                  List[(MicroCheckerLib.nat,
 Option[(cmd[A], MicroCheckerLib.nat)])]]]) =>
                           MicroCheckerLib.finfun[MicroCheckerLib.nat,
           MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                   List[(MicroCheckerLib.nat,
  Option[(cmd[A], MicroCheckerLib.nat)])]]],
                        x1: acc_state_ext[A, B]):
      acc_state_ext[A, B]
  =
  (onebsa, x1) match {
  case (onebsa,
         acc_state_exta(id, leader, acceptors, ballot, decided, vote,
                         last_ballot, onebs, twobs, next_inst, last_decision,
                         working_instances, commit_buffer, last_committed,
                         snapshot_reference, snapshot_proposal, more))
    => acc_state_exta[A, B](id, leader, acceptors, ballot, decided, vote,
                             last_ballot, onebsa(onebs), twobs, next_inst,
                             last_decision, working_instances, commit_buffer,
                             last_committed, snapshot_reference,
                             snapshot_proposal, more)
}

def update_onebs[A : MicroCheckerLib.equal](s: acc_state_ext[A, Unit],
     bal: MicroCheckerLib.nat, a2: MicroCheckerLib.nat,
     last_vs:
       MicroCheckerLib.finfun[MicroCheckerLib.nat,
                               Option[(cmd[A], MicroCheckerLib.nat)]]):
      acc_state_ext[A, Unit]
  =
  {
    id[A, Unit](s)
    val combiner:
          ((List[(MicroCheckerLib.nat, Option[(cmd[A], MicroCheckerLib.nat)])],
            Option[(cmd[A], MicroCheckerLib.nat)])) =>
            List[(MicroCheckerLib.nat, Option[(cmd[A], MicroCheckerLib.nat)])]
      = ((a: (List[(MicroCheckerLib.nat,
                     Option[(cmd[A], MicroCheckerLib.nat)])],
               Option[(cmd[A], MicroCheckerLib.nat)]))
           =>
          {
            val (xs, y):
                  (List[(MicroCheckerLib.nat,
                          Option[(cmd[A], MicroCheckerLib.nat)])],
                    Option[(cmd[A], MicroCheckerLib.nat)])
              = a;
            (if (MicroCheckerLib.membera[(MicroCheckerLib.nat,
   Option[(cmd[A], MicroCheckerLib.nat)])](xs, (a2, y)))
              xs else (a2, y) :: xs)
          })
    val pair_map:
          MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                  (List[(MicroCheckerLib.nat,
  Option[(cmd[A], MicroCheckerLib.nat)])],
                                    Option[(cmd[A], MicroCheckerLib.nat)])]
      = MicroCheckerLib.finfun_Diag[MicroCheckerLib.nat,
                                     List[(MicroCheckerLib.nat,
    Option[(cmd[A], MicroCheckerLib.nat)])],
                                     Option[(cmd[A],
      MicroCheckerLib.nat)]](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                   MicroCheckerLib.finfun[MicroCheckerLib.nat,
   List[(MicroCheckerLib.nat,
          Option[(cmd[A], MicroCheckerLib.nat)])]]](onebs[A, Unit](s), bal),
                              last_vs)
    val at_bal:
          MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                  List[(MicroCheckerLib.nat,
 Option[(cmd[A], MicroCheckerLib.nat)])]]
      = MicroCheckerLib.finfun_comp[(List[(MicroCheckerLib.nat,
    Option[(cmd[A], MicroCheckerLib.nat)])],
                                      Option[(cmd[A], MicroCheckerLib.nat)]),
                                     List[(MicroCheckerLib.nat,
    Option[(cmd[A], MicroCheckerLib.nat)])],
                                     MicroCheckerLib.nat](combiner, pair_map);
    onebs_update[A, Unit](((_: MicroCheckerLib.finfun[MicroCheckerLib.nat,
               MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                       List[(MicroCheckerLib.nat,
      Option[(cmd[A], MicroCheckerLib.nat)])]]])
                             =>
                            MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
                   MicroCheckerLib.finfun[MicroCheckerLib.nat,
   List[(MicroCheckerLib.nat,
          Option[(cmd[A],
                   MicroCheckerLib.nat)])]]](onebs[A, Unit](s), bal, at_bal)),
                           s)
  }

def receive_1b[A : MicroCheckerLib.equal](last_vs:
    MicroCheckerLib.finfun[MicroCheckerLib.nat,
                            Option[(cmd[A], MicroCheckerLib.nat)]],
   bal: MicroCheckerLib.nat, a2: MicroCheckerLib.nat,
   s: acc_state_ext[A, Unit]):
      (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
  =
  {
    val a: MicroCheckerLib.nat = id[A, Unit](s);
    (if (Optiona.equal_optiona[MicroCheckerLib.nat](Some[MicroCheckerLib.nat](bal),
             ballot[A, Unit](s)))
      {
        val s1: acc_state_ext[A, Unit] = update_onebs[A](s, bal, a2, last_vs);
        (if (one_b_quorum_received[A, Unit](bal, s1))
          {
            val h: MicroCheckerLib.finfun[MicroCheckerLib.nat, Option[cmd[A]]] =
              highest_voted[A](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                     MicroCheckerLib.finfun[MicroCheckerLib.nat,
     List[(MicroCheckerLib.nat,
            Option[(cmd[A], MicroCheckerLib.nat)])]]](onebs[A, Unit](s1), bal))
            val max_i: MicroCheckerLib.nat =
              {
                val l: List[MicroCheckerLib.nat] =
                  MicroCheckerLib.finfun_to_list[MicroCheckerLib.nat,
          List[(MicroCheckerLib.nat,
                 Option[(cmd[A],
                          MicroCheckerLib.nat)])]](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
 MicroCheckerLib.finfun[MicroCheckerLib.nat,
                         List[(MicroCheckerLib.nat,
                                Option[(cmd[A],
 MicroCheckerLib.nat)])]]](onebs[A, Unit](s1), bal));
                (if (MicroCheckerLib.nulla[MicroCheckerLib.nat](l))
                  MicroCheckerLib.zero_nat
                  else MicroCheckerLib.hd[MicroCheckerLib.nat](MicroCheckerLib.rev[MicroCheckerLib.nat](l)))
              }
            val s2: acc_state_ext[A, Unit] =
              leader_update[A, Unit](((_: Boolean) => true), s1)
            val s3: acc_state_ext[A, Unit] =
              next_inst_update[A, Unit](((_: MicroCheckerLib.nat) =>
  MicroCheckerLib.plus_nat(max_i, MicroCheckerLib.one_nat)),
 s2)
            val twoa_is: List[MicroCheckerLib.nat] =
              MicroCheckerLib.upt(MicroCheckerLib.one_nat,
                                   MicroCheckerLib.plus_nat(max_i,
                     MicroCheckerLib.one_nat))
            val s4: acc_state_ext[A, Unit] =
              MicroCheckerLib.fold[MicroCheckerLib.nat,
                                    acc_state_ext[A,
           Unit]](((i: MicroCheckerLib.nat) => (sa: acc_state_ext[A, Unit]) =>
                    twobs_update[A, Unit](((_:
      MicroCheckerLib.finfun[MicroCheckerLib.nat,
                              MicroCheckerLib.finfun[MicroCheckerLib.nat,
              List[MicroCheckerLib.nat]]])
     =>
    MicroCheckerLib.finfun_update_code[MicroCheckerLib.nat,
MicroCheckerLib.finfun[MicroCheckerLib.nat,
                        List[MicroCheckerLib.nat]]](twobs[A, Unit](sa), i,
             MicroCheckerLib.finfun_update[MicroCheckerLib.nat,
    List[MicroCheckerLib.nat]](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                     MicroCheckerLib.finfun[MicroCheckerLib.nat,
     List[MicroCheckerLib.nat]]](twobs[A, Unit](sa), i),
                                bal, List(a)))),
   sa)),
                   twoa_is, s3)
            val msgs: List[msg[A]] =
              MicroCheckerLib.map[MicroCheckerLib.nat,
                                   msg[A]](((i: MicroCheckerLib.nat) =>
     (MicroCheckerLib.finfun_apply[MicroCheckerLib.nat, Option[cmd[A]]](h, i)
        match {
        case None => Phase2a[A](i, bal, NoOp[A](), a)
        case Some(v) => Phase2a[A](i, bal, v, a)
      })),
    twoa_is)
            val pckts: List[MicroCheckerLib.fset[packet[A]]] =
              MicroCheckerLib.map[msg[A],
                                   MicroCheckerLib.fset[packet[A]]](((b: msg[A])
                               =>
                              send_all[A, Unit, A](s, a, b)),
                             msgs);
            (s4, MicroCheckerLib.fold[MicroCheckerLib.fset[packet[A]],
                                       MicroCheckerLib.fset[packet[A]]](((aa:
                                    MicroCheckerLib.fset[packet[A]])
                                   =>
                                  (b: MicroCheckerLib.fset[packet[A]]) =>
                                  MicroCheckerLib.sup_fset[packet[A]](aa, b)),
                                 pckts, MicroCheckerLib.bot_fset[packet[A]]))
          }
          else (s1, MicroCheckerLib.bot_fset[packet[A]]))
      }
      else (s, MicroCheckerLib.bot_fset[packet[A]]))
  }

def last_ballot[A, B](x0: acc_state_ext[A, B]):
      MicroCheckerLib.finfun[MicroCheckerLib.nat, Option[MicroCheckerLib.nat]]
  =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => last_ballot
}

def receive_1a[A : MicroCheckerLib.equal](l: MicroCheckerLib.nat,
   b: MicroCheckerLib.nat, s: acc_state_ext[A, Unit]):
      (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
  =
  {
    val bal: Option[MicroCheckerLib.nat] = ballot[A, Unit](s)
    val a: MicroCheckerLib.nat = id[A, Unit](s);
    (if (Optiona.is_none[MicroCheckerLib.nat](bal) ||
           MicroCheckerLib.less_nat(Optiona.the[MicroCheckerLib.nat](bal), b))
      {
        val combiner:
              ((Option[cmd[A]], Option[MicroCheckerLib.nat])) =>
                Option[(cmd[A], MicroCheckerLib.nat)]
          = ((aa: (Option[cmd[A]], Option[MicroCheckerLib.nat])) =>
              {
                val (vo, bo): (Option[cmd[A]], Option[MicroCheckerLib.nat]) =
                  aa;
                Optiona.bind[cmd[A],
                              (cmd[A],
                                MicroCheckerLib.nat)](vo,
               ((v: cmd[A]) =>
                 Optiona.bind[MicroCheckerLib.nat,
                               (cmd[A],
                                 MicroCheckerLib.nat)](bo,
                ((ba: MicroCheckerLib.nat) =>
                  Some[(cmd[A], MicroCheckerLib.nat)]((v, ba))))))
              })
        val x: MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                       (Option[cmd[A]],
 Option[MicroCheckerLib.nat])]
          = MicroCheckerLib.finfun_Diag[MicroCheckerLib.nat, Option[cmd[A]],
 Option[MicroCheckerLib.nat]](vote[A, Unit](s), last_ballot[A, Unit](s))
        val last_votes:
              MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                      Option[(cmd[A], MicroCheckerLib.nat)]]
          = MicroCheckerLib.finfun_comp[(Option[cmd[A]],
  Option[MicroCheckerLib.nat]),
 Option[(cmd[A], MicroCheckerLib.nat)], MicroCheckerLib.nat](combiner, x)
        val msg_1b: msg[A] = Phase1b[A](last_votes, b, a)
        val packet: packet[A] = Packet[A](a, l, msg_1b)
        val state: acc_state_ext[A, Unit] =
          ballot_update[A, Unit](((_: Option[MicroCheckerLib.nat]) =>
                                   Some[MicroCheckerLib.nat](b)),
                                  s);
        (state,
          MicroCheckerLib.finsert[packet[A]](packet,
      MicroCheckerLib.bot_fset[packet[A]]))
      }
      else (s, MicroCheckerLib.bot_fset[packet[A]]))
  }

def mp_transit[A : MicroCheckerLib.equal,
                B](s: mp_state_ext[A, B], x1: mp_action[A]):
      mp_state_ext[A, B]
  =
  (s, x1) match {
  case (s, Receive_fwd(src, dest, v)) =>
    (if (MicroCheckerLib.fmember[packet[A]](Packet[A](src, dest, Fwd[A](v)),
     network[A, B](s)))
      {
        val (new_s, ps):
              (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
          = receive_fwd[A](v, MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                    acc_state_ext[A, Unit]](node_states[A, B](s), dest));
        update_state[A, B](dest, new_s, ps, s)
      }
      else s)
  case (s, Propose(a, Cmd(v))) =>
    {
      val (new_s, ps): (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
        = propose[A](v, MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
              acc_state_ext[A, Unit]](node_states[A, B](s), a));
      update_state[A, B](a, new_s, ps, s)
    }
  case (s, Receive_1a_send_1b(src, dest, b)) =>
    (if (MicroCheckerLib.fmember[packet[A]](Packet[A](src, dest,
               Phase1a[A](src, b)),
     network[A, B](s)))
      {
        val (new_s, ps):
              (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
          = receive_1a[A](src, b,
                           MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                 acc_state_ext[A, Unit]](node_states[A, B](s), dest));
        update_state[A, B](dest, new_s, ps, s)
      }
      else s)
  case (s, Send_1as(l)) =>
    {
      val (new_s, ps): (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
        = send_1a[A](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
           acc_state_ext[A, Unit]](node_states[A, B](s), l));
      update_state[A, B](l, new_s, ps, s)
    }
  case (s, Receive_1b(src, l, vs, b)) =>
    (if (MicroCheckerLib.fmember[packet[A]](Packet[A](src, l,
               Phase1b[A](vs, b, src)),
     network[A, B](s)))
      {
        val (new_s, ps):
              (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
          = receive_1b[A](vs, b, src,
                           MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                 acc_state_ext[A, Unit]](node_states[A, B](s), l));
        update_state[A, B](l, new_s, ps, s)
      }
      else s)
  case (s, Receive_2b(a, l, i, b, cm)) =>
    (if (MicroCheckerLib.fmember[packet[A]](Packet[A](a, l,
               Phase2b[A](i, b, a, cm)),
     network[A, B](s)))
      {
        val (new_s, ps):
              (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
          = receive_2b[A](i, b, a, cm,
                           MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                 acc_state_ext[A, Unit]](node_states[A, B](s), l));
        update_state[A, B](l, new_s, ps, s)
      }
      else s)
  case (s, Receive_2a_send_2b(l, dest, i, b, cm)) =>
    (if (MicroCheckerLib.fmember[packet[A]](Packet[A](l, dest,
               Phase2a[A](i, b, cm, l)),
     network[A, B](s)))
      {
        val (new_s, ps):
              (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
          = receive_2a[A](i, b, cm, dest,
                           MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                 acc_state_ext[A, Unit]](node_states[A, B](s), dest));
        update_state[A, B](dest, new_s, ps, s)
      }
      else s)
  case (s, Learn(a, i, Cmd(v))) =>
    (learn[A](i, v,
               MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
     acc_state_ext[A, Unit]](node_states[A, B](s), a))
       match {
       case None => s
       case Some((new_s, ps)) => update_state[A, B](a, new_s, ps, s)
     })
}

def get_decided[A](s: mp_state_ext[A, Unit], acc: MicroCheckerLib.nat,
                    i: MicroCheckerLib.nat):
      Option[cmd[A]]
  =
  MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
                                Option[cmd[A]]](decided[A,
                 Unit](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
             acc_state_ext[A, Unit]](node_states[A, Unit](s), acc)),
         i)

def process_msg[A : MicroCheckerLib.equal](x0: msg[A],
    s: acc_state_ext[A, Unit]):
      (acc_state_ext[A, Unit], MicroCheckerLib.fset[packet[A]])
  =
  (x0, s) match {
  case (Phase1a(l, b), s) => receive_1a[A](l, b, s)
  case (Phase1b(lvs, b, a), s) => receive_1b[A](lvs, b, a, s)
  case (Phase2a(i, b, cm, l), s) => receive_2a[A](i, b, cm, l, s)
  case (Phase2b(i, b, a, cm), s) => receive_2b[A](i, b, a, cm, s)
  case (Vote(i, cm), s) => sys.error("undefined")
  case (Fwd(v), s) => receive_fwd[A](v, s)
}

def inst_constraint[A, B](s: mp_state_ext[A, B], bound: MicroCheckerLib.nat):
      Boolean
  =
  {
    val n_s: MicroCheckerLib.finfun[MicroCheckerLib.nat, acc_state_ext[A, Unit]]
      = node_states[A, B](s)
    val insts: MicroCheckerLib.finfun[MicroCheckerLib.nat, MicroCheckerLib.nat]
      = MicroCheckerLib.finfun_comp[acc_state_ext[A, Unit], MicroCheckerLib.nat,
                                     MicroCheckerLib.nat](((a:
                      acc_state_ext[A, Unit])
                     =>
                    next_inst[A, Unit](a)),
                   n_s)
    val as: List[MicroCheckerLib.nat] =
      MicroCheckerLib.finfun_to_list[MicroCheckerLib.nat,
                                      MicroCheckerLib.nat](insts)
    val inst_list: List[MicroCheckerLib.nat] =
      MicroCheckerLib.map[MicroCheckerLib.nat,
                           MicroCheckerLib.nat](((a: MicroCheckerLib.nat) =>
          MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
MicroCheckerLib.nat](insts, a)),
         as)
    val inst_list_sorted: List[MicroCheckerLib.nat] =
      MicroCheckerLib.sort_key[MicroCheckerLib.nat,
                                MicroCheckerLib.nat](((x: MicroCheckerLib.nat)
                =>
               x),
              inst_list)
    val max_inst: MicroCheckerLib.nat =
      (if (MicroCheckerLib.nulla[MicroCheckerLib.nat](inst_list_sorted))
        MicroCheckerLib.one_nat
        else MicroCheckerLib.nth[MicroCheckerLib.nat](inst_list_sorted,
               MicroCheckerLib.minus_nat(MicroCheckerLib.size_list[MicroCheckerLib.nat].apply(inst_list_sorted),
  MicroCheckerLib.one_nat)));
    MicroCheckerLib.less_eq_nat(max_inst, bound)
  }

def ballot_constraint[A, B](s: mp_state_ext[A, B], bound: MicroCheckerLib.nat):
      Boolean
  =
  {
    val n_s: MicroCheckerLib.finfun[MicroCheckerLib.nat, acc_state_ext[A, Unit]]
      = node_states[A, B](s)
    val bals: MicroCheckerLib.finfun[MicroCheckerLib.nat,
                                      Option[MicroCheckerLib.nat]]
      = MicroCheckerLib.finfun_comp[acc_state_ext[A, Unit],
                                     Option[MicroCheckerLib.nat],
                                     MicroCheckerLib.nat](((a:
                      acc_state_ext[A, Unit])
                     =>
                    ballot[A, Unit](a)),
                   n_s)
    val as: List[MicroCheckerLib.nat] =
      MicroCheckerLib.finfun_to_list[MicroCheckerLib.nat,
                                      Option[MicroCheckerLib.nat]](bals)
    val bal_list: List[Option[MicroCheckerLib.nat]] =
      MicroCheckerLib.map[MicroCheckerLib.nat,
                           Option[MicroCheckerLib.nat]](((a:
                    MicroCheckerLib.nat)
                   =>
                  MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
        Option[MicroCheckerLib.nat]](bals, a)),
                 as)
    val bal_list_sorted: List[Option[MicroCheckerLib.nat]] =
      MicroCheckerLib.sort_key[Option[MicroCheckerLib.nat],
                                Option[MicroCheckerLib.nat]](((x:
                         Option[MicroCheckerLib.nat])
                        =>
                       x),
                      bal_list)
    val max_bal: Option[MicroCheckerLib.nat] =
      (if (MicroCheckerLib.nulla[Option[MicroCheckerLib.nat]](bal_list_sorted))
        None
        else MicroCheckerLib.nth[Option[MicroCheckerLib.nat]](bal_list_sorted,
                       MicroCheckerLib.minus_nat(MicroCheckerLib.size_list[Option[MicroCheckerLib.nat]].apply(bal_list_sorted),
          MicroCheckerLib.one_nat)));
    MicroCheckerLib.less_eq_option[MicroCheckerLib.nat](max_bal,
                 Some[MicroCheckerLib.nat](bound))
  }

def last_decision[A, B](x0: acc_state_ext[A, B]): Option[MicroCheckerLib.nat] =
  x0 match {
  case acc_state_exta(id, leader, acceptors, ballot, decided, vote, last_ballot,
                       onebs, twobs, next_inst, last_decision,
                       working_instances, commit_buffer, last_committed,
                       snapshot_reference, snapshot_proposal, more)
    => last_decision
}

def get_last_decision[A, B](s: acc_state_ext[A, B]): cmd[A] =
  Optiona.the[cmd[A]](MicroCheckerLib.finfun_apply[MicroCheckerLib.nat,
            Option[cmd[A]]](decided[A, B](s),
                             Optiona.the[MicroCheckerLib.nat](last_decision[A,
                                     B](s))))

} /* object MultiPaxos4 */
