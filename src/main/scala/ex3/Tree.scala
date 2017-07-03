package ex3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Exercise 3.25
    * =============
    * Write a function `size` that counts the number of nodes (leaves and branches)
    * in a tree.
    */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /**
    * Exercise 3.26
    * =============
    * Write a function `maximum` that returns the maximum elements in a `Tree[Int]`.
    */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
    * Exercise 3.27
    * =============
    * Write a function `depth` that returns the maximum path length from the root of a
    * tree to any leaf.
    */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  /**
    * Exercise 3.28
    * =============
    * Write a function `map`, analogous to the method of the same name on `List`, that
    * modifies each element in a tree with a given function.
    */
  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }

  /**
    * Exercise 3.29
    * =============
    * Generalize `size`, `maximum`, `depth` and `map`, writing a new function `fold` that
    * abstracts over their similarities. Reimplement them in terms of this more general function.
    */
  def fold[A, B](tree: Tree[A], z: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(value) => z(value)
    case Branch(l, r) => f(fold(l, z)(f), fold(r, z)(f))
  }

  def size2[A](tree: Tree[A]): Int = fold[A, Int](tree, _ => 1)((x, y) => x + y + 1)
  def maximum2(tree: Tree[Int]): Int = fold[Int, Int](tree, x => x)((x, y) => x max y)
  def depth2[A](tree: Tree[A]): Int = fold[A, Int](tree, _ => 1)((x, y) => 1 + (x max y))
  def map2[A, B](tree: Tree[A], f: A => B): Tree[B] = fold[A, Tree[B]](tree, x => Leaf(f(x)))((x, y) => Branch(x, y))
}