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
