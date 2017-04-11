  // Exercise 3.1: What is the output?
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // pattern matches here! so asnwer is 3
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
    
  // Exercise 3.2
  def tail[A](elems: List[A]): List[A] = elems match {
    case Nil => Nil
    case Cons(_, remaining) => remaining
  }

  // Exercise 3.3
  def setHead[A](elems: List[A], elem: A): List[A] = elems match {
    case Nil => Nil
    case Cons(_, tail) => Cons(elem, tail)
  }

  // Exercise 3.4
  def drop[A](elems: List[A], n: Int): List[A] = elems match {
    case Nil => Nil
    case Cons(_, _) if n <= 0 => elems
    case Cons(_, tail) => drop(tail, n - 1) 
  }
