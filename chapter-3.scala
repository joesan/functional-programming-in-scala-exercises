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

  // Exercise 3.5
  def dropWhile[A](elems: List[A], f: A => Boolean): List[A] = elems match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => elems
  }

  // Exercise 3.6 - Remember the accumulator pattern from Martin Oedersky's course in Coursera!!
  def init[A](elems: List[A], acc: List[A]): List[A] = elems match {
    case Nil => Nil
    case Cons(h, Nil) => elems // holy shit!! we did not yet hit reverse!!!
    case Cons(h, t) => init(t, acc += h)
  }