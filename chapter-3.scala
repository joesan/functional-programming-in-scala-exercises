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

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  // Exercise 3.10 - A tail recursive foldLeft
  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(head, z))(f)
  }

  // Exercise 3.11
  def sumFoldLeft(elems: List[Int]) = foldLeft(elems, 0)(_ + _)
  def productFoldLeft(elems: List[Double]) = foldLeft(elems, 1.0)(_ * _)
  def lengthFoldLeft[A](elems: List[A]): Int = foldLeft(elems, 0)((acc, h) => acc + 1)

  // Exercise 3.12
  def reverse[A](elems: List[A]): List[A] = {
    foldLeft(elems, List.empty[A])((acc, h) => Cons(h, acc))
  }

  // Exercise 3.13
  def foldRightUsingFoldLeft[A, B](elems: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(elems), z)((b, a) => f(a, b))
  }

  // Exercise 3.14
  def appendUsingFoldLeft[A](x: List[A], y: List[A]): List[A] = {
    foldLeft(x, y)(Cons(_,_))
  }

  // Exercise 3.15
  def concat[A](elems: List[List[A]]): List[A] = {
    foldRight(elems, List.empty[A])(append)
  }

  // Exercise 3.16
  def transformByAdding(elems: List[Int])(toAdd: Int): List[Int] = {
    foldLeft(elems, List.empty[Int])((acc, head) => Cons(head + toAdd, acc))
  }

  // Exercise 3.17
  def doubleToString(elems: List[Double]): List[String] = {
    foldLeft(elems, List.empty[Double])((acc, head) => Cons(head.toString, acc))
  }

  // Exercise 3.18
  def map[A, B](elems: List[A])(f: A => B): List[B] = {
    
    @scala.annotation.tailrec
    def tailRecMap(elems: List[A], acc: List[B]): List[B] = elems match {
      case Nil => acc
      case Cons(head, tail) => tailRecMap(tail, Cons(f(head), acc))
    }
    
    tailRecMap(elems, List.empty[B])
  }

  // Exercise 3.19
  def filter[A](elems: List[A])(f: A => Boolean): List[A] = {

    @scala.annotation.tailrec
    def tailRecFilter(elems: List[A], acc: List[A]): List[A] = elems match {
      case Nil => acc
      case Cons(head, tail) if f(head) => tailRecFilter(tail, Cons(head, acc))
      case Cons(_, tail) => tailRecFilter(tail, acc)
    }

    tailRecFilter(elems, List.empty[A])
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = 
    as.foldRight(List.empty[B])((acc, listOfB) => {
      f(acc) ++ listOfB
  })

  // Exercise 3.21
  def filterUsingFlatMap[A](elems: List[A])(f: A => Boolean): List[A] = {
    flatMap(elems)(a => if (f(a)) List(a) else Nil)
  }

  // Exercise 3.22
  def addLists(x: List[Int], y: List[Int]): List[Int] = (x, y) match {
    case (Nil, elems) => elems
    case (elems, Nil) => elems
    case (Cons(xHead, xTail), Cons(yHead, yTail)) => Cons(xHead + yHead, addLists(x, y))
  }

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
