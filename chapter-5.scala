trait Stream[+A] { self =>

  // Ecercise 5.1
  def toList: List[A] = {
    @tailrec
    def convert(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(head, tail) => convert(tail(), head() :: acc)
      case _ => acc
    }
    convert(self, List.empty[A]).reverse
  }
}
