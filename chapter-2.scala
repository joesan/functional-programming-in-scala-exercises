// Exercise 2.2
 def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def check(next: Array[A]): Boolean = next.length match {
      case a if a == 0 || a == 1 =>
        true
      case x if x == 2 =>
        ordered(next(0), next(1))
      case d if ordered(next(0), next(1)) =>
        check(next.drop(1))
      case _ =>
        false
    }

    check(as)
  }

  println(isSorted(Array(1, 3, 2), (x: Int, y: Int) => x < y)) // prints false
  println(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x < y)) // prints true
  println(isSorted(Array(1), (x: Int, y: Int) => x < y)) // prints true
  println(isSorted(Array(1, 2), (x: Int, y: Int) => x < y)) // prints true
