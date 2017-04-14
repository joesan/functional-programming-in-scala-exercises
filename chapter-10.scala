  // Exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }
