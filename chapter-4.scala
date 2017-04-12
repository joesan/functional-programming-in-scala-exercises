  // Exercise 4.1
  def map[A](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _       => None
  }
  
  def flatMap[A](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _       => None
  }
  
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None    => default
  }

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    val mean = mean(xs)
    mean.flatMap(elem => mean(elem.map(x => math.pow(x - mean, 2))))
  }

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) | (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.contains(None)) None
    else Some(a.flatten(x => x))
  }
