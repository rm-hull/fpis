
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
    
   /**
    * Exercise 3.1
    * ============
    * <see test>
    */
    
    
   /**
    * Exercise 3.2
    * ============
    * Implement the function `tail` for removing the first element of a `List`.
    * Note that the function takes constant time. What are the different choices
    * you could make in your implementation if the `List` is `Nil`?
    */
   def tail[A](as: List[A]) = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }
  
  /**
   * Exercise 3.3
   * ============
   * Using the same idea, implement the function `setHead` for replacing the first
   * element of a list with a different value.
   */
  def setHead[A](newHead: A, as: List[A]) = as match {
    case Nil => List(newHead)
    case Cons(x, xs) => Cons(newHead, xs)
  }
  
  /**
   * Exercise 3.4
   * ============
   * Generalize `tail` to the function `drop`, which removes the first `n` elements
   * from a list. Note that this function takes time proportional only to the number
   * of elements being dropped--we don't need to make a copy of the entire `List`.
   */
  def drop[A](l: List[A], n: Int): List[A] = { 
    require(n >= 0)
    n match {
      case 0 => l
      case _ => l match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n-1)
      }
    }
  }
  
  def tail_v2[A](as: List[A]) = drop(as, 1)
  
  /**
   * Exercise 3.5
   * ============
   * Implement `dropWhile`, which removes elements from the `List` prefix as long
   * as they match a predicate.
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }
}