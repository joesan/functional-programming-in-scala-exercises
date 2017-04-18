  // Exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int) = a + b
    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int) = a * b
    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b
    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a && b
    val zero = true
  }

  // Exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {  
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  // Exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 andThen a2
    def zero = (a: A) => a
  }

  // Exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldRight(m.zero)((b, a) => m.op(b, f(a)) 
  }
                         
  // Exercise 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B {
   
    // I could have used the foldMap from Exercise 10.5 as well!!!
    def fold[A](v: IndexedSeq[A]): B = {
      as.foldRight(m.zero)((b, a) => m.op(b, f(a))
    }
    
    // split the List in two halves
    val length = v.length
    val (firstHalf, secondHalf) = if (length % 2 == 0) {
             (l.take(length / 2), l diff l.take(length / 2))
           }
           else (l.take((length - 1) / 2), l diff l.take((length - 1) / 2))
    
    m.op(fold(firstHalf), fold(secondHalf))
  }
