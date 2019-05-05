sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

// All utility methods around the tree is to be found in this companion
object Tree {
  
  def empty[A]: Tree[A] = Empty
  
  def node[A](value: A, left: Tree[A] = Empty, right: Tree[A] = Empty): Tree[A] =
    Node(value, left, right)
    
  def fold[A, Z](t: Tree[A])(leaf: Z, node: (Z, A, A)) => Z): Z = t match {
    case Empty => leaf
    case Node(value, left, right) => node(fold(left)(value, node), leaf, fold(right)(value, node))
  }
}
