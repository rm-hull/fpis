package ex5

sealed trait Stream[+A] {

  /**
    * Exercise 5.1: Write a function to convert a `Stream` to a `List`,
    * which will force its evaluation and let you look at the REPL.
    */
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  /**
    * Exercise 5.2: Write the function `take(n)` for returning the first
    * `n` elements of a `Stream` and `drop(n)` for skipping the first
    * `n` elements of a `Stream`.
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
    * Exercise 5.3: Write the function `takeWhile` for returning all starting
    * elements of a `Stream` that matches the given predicate.
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
    * Exercise 5.4: Implement `forAll`, which checks that all elements in the
    * `Stream` match a given predicate. Your implementation should terminate
    * the traversal as soon as it encounters a nonmatching value.
    */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
    * Exercise 5.5: Use `foldRight` to implement `takeWhile`
    */
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  /**
    * Exercise 5.6: Implement `headOption` using `foldRight`
    */
  def headOption(): Option[A] =
    foldRight[Option[A]](None)((a, b) => Some(a))

  /**
    * Exercise 5.7: Implement `map`, `filter`, `append`, and `flatMap` using
    * `foldRight`. The `append` method should be non-strict in its argument.
    */
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Stream.empty)((a, b) => Stream.cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Stream.empty)((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other)((a, b) => Stream.cons(a, b))

  /**
    * Exercise 5.13: Use `unfold` to implement `map`, `take`, `takeWhile`,
    * `zipWith` (as in chapter 3), and `zipAll`
    */
  def map2[B](f: A => B): Stream[B] = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  /**
    * Exercise 5.8: Generalize `ones` slightly to the function `constant`,
    * which returns an infinite `Stream` of a given value.
    */
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  /**
    * Exercise 5.9: Write a function that generates an infinite stream of
    * integers, starting from `n`, then `n + 1`, `n + 2`, and so on.
    */
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  /**
    * Exercise 5.10: Write a function `fibs` that generates the infinite
    * stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
    */
  def fibs: Stream[Int] = {
    def fib0(a: Int, b: Int): Stream[Int] = Stream.cons(a, fib0(b, a + b))

    fib0(0, 1)
  }

  /**
    * Exercise 5.11: Write a more-general stream-building function called
    * `unfold`. It takes an initial state, and a function for producing both
    * the next state and the next value in the generated stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((nextVal, nextState)) => Stream.cons(nextVal, unfold(nextState)(f))
      case None => Stream.empty
    }

  /**
    * Exercise 5.12: Write `fibs`, `from`, `constant`, and `ones` in terms
    * of `unfold`.
    */
  def fibs2: Stream[Int] =
    unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

  def from2(n: Int): Stream[Int] =
    unfold[Int, Int](n)(i => Some(i, i + 1))

  def constant2[A](a: A): Stream[A] =
    unfold[A, AnyRef](null)(x => Some((a, x)))

  val ones2: Stream[Int] =
    unfold[Int, AnyRef](null)(x => Some((1, x)))


}
