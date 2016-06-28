import scala.annotation.tailrec

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
     **/
    
    
   /**
     * Exercise 3.2
     * ============
     * Implement the function `tail` for removing the first element of a `List`.
     * Note that the function takes constant time. What are the different choices
     * you could make in your implementation if the `List` is `Nil`?
     **/
   def tail[A](as: List[A]) = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }
  
  /**
    * Exercise 3.3
    * ============
    * Using the same idea, implement the function `setHead` for replacing the first
    * element of a list with a different value.
    **/
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
    **/
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
    **/
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  /**
    * Exercise 3.6
    * ============
    * Implement a function, `init`, that returns a `List` consisting of all but the
    * last elements of a `List`. So given `List(1,2,3,4)`, `init` will return
    * `List(1,2,3)`. Why can't this function be implemented in constant time like
    * `tail`?
    **/
  // All linked list elements must be traversed to get to the last element,
  // hence the function runs in linear time
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  /**
    * Exercise 3.7
    * ============
    * Can `product`, implemented using `foldRight`, immediately halt the
    * recursion and return 0.0 if it encounters 0.0? Why or why not?
    **/
  // No, because foldRight pushes onto the stack first, and computes `f`
  // only when the list is fully exhausted, by which point it is too late
  // to short-circuit the recursion.

  /**
    * Exercise 3.8
    * ============
    * See what happens when you pass `Nil` and `Cons` themselves to `foldRight`,
    * like this:
    *
    *   foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    *
    * What do you think this says about the relationship between `foldRight`
    * and the data constructors of `List`?
    */
  // foldRight can be used to build lists in the same left-to-right
  // ordering as linked list traversal

  /**
    * Exercise 3.9
    * ============
    * Compute the length of a list using `foldRight`
    **/
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => b + 1)

  /**
    * Exercise 3.10
    * =============
    * ... then write another general list-recursion function, `foldLeft`, that
    * is tail-recursive, using the techniques we discussed in the previous chapter
    **/
  def foldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {

    @tailrec
    def go(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(x, acc))
    }

    go(as, z)
  }

  /**
    * Exercise 3.11
    * =============
    * Write `sum`, `product`, and a function to compute the length of a list
    * using `foldLeft`.
    */
  def sum3(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product3(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def length3[A](as: List[A]): Int = foldLeft(as, 0)((a, b) => b + 1)

  /**
    * Exercise 3.12
    * =============
    * Write a function that returns the reverse of a list (given `List(1,2,3)`
    * it returns `List(3,2,1)`. See if you can write it using a fold
    */
  // Can't be done with foldRight!?!
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])(Cons(_,_))

  /**
    * Exercise 3.13
    * =============
    * Can you write `foldLeft` in terms of `foldRight`? How about the other
    * way around?
    */
  // TBD

  /**
    * Exercise 3.14
    * =============
    * Implement `append` in terms of either `foldLeft` or `foldRight`
    */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  /**
    * Exercise 3.15
    * =============
    * Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists. Try to
    * use functions we have already defined.
    */
  def flatten[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  /**
    * Exercise 3.16
    * =============
    * Write a function that transforms a list of integers by adding 1 to each
    * element.
    */
  def add1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

  /**
    * Exercise 3.17
    * =============
    * Write a function that turns each value in a `List[Double]` into a
    * `String`.
    */
  def dbl2str(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))

  /**
    * Exercise 3.18
    * =============
    * Write a function `map` that generalizes modifying each element in a list
    * while maintaining the structure of the list.
    */
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  /**
    * Exercise 3.19
    * =============
    * Write a function `filter` that remove elements from a list unless they
    * satisfy a given predicate. Use it to remove all odd numbers from a
    * `List[Int]`.
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
}