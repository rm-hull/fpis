package ex6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newseed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newseed)
    val n = (newseed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  /**
    * Exercise 6.3: Write functions to generate an `(Int, Double)` pair, a `(Double, Int)`
    * pair, and a `(Double, Double, Double)` 3-tuple.
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, next) = rng.nextInt
    val (d, next2) = double(next)
    ((i, d), next2)
  }

  /**
    * Exercise 6.2: Write a function to generate a `Double` between `0` and `1`, not
    * including `1`.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, next) = nonNegativeInt(rng)
    (i / (Int.MaxValue + 1.0), next)
  }

  /**
    * Exercise 6.1: Write a function that uses `RNG.nextInt` to generate a random
    * integer between `0` and `Int.maxValue` (inclusive). Make sure to handle the
    * corner case when `nextInt` returns `Int.minValue`, which doesn't have a
    * non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, next) if i >= 0 => (i, next)
    case (i, next) => (-(i + 1), next)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, next) = rng.nextInt
    val (d, next2) = double(next)
    ((d, i), next2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, next1) = double(rng)
    val (d2, next2) = double(next1)
    val (d3, next3) = double(next2)
    ((d1, d2, d3), next3)
  }

  /**
    * Exercise 6.4: Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    Range(0, count).foldLeft((List.empty[Int], rng)) {
      case ((lst, rng), _) =>
        val (i, next) = rng.nextInt
        (lst :+ i, next)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * Exercise 6.5: Use `map` to reimplement `double` in a more elegant way.
    */
  def double2: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue + 1.0))
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  /**
    * Exercise 6.7: If you can combine two RNG transitions, you should be able to combine
    * a whole list of them. Implement `sequence` for combining a `List` of transitions into
    * a single transition. Use it to reimplement the `ints` function you wrote before. For
    * the latter, you can use the standard library function `List.fill(n)(x)` to make a list
    * with `x` repeated `n` times.
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldLeft((List.empty[A], rng)) {
        case ((lst, rng), b) =>
          val (i, next) = b(rng)
          (lst :+ i, next)
      }
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit[List[A]](List.empty[A])) {
      (f, acc) => map2(f, acc)(_ :: _)
    }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /**
    * Exercise 6.6: Write the implementation of `map2` based on the following signature.
    * This function takes two actions, `ra` and `rb`, and a function `f` for combining
    * their results, and returns a new action that combines them:
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng2)
      else nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
    }

  /**
    * Exercise 6.9: Reimplement `map` and `map2` in terms of `flatMap`. The fact that this
    * is possible is what we're referring to when we say that `flatMap` is _more powerful_
    * than `map` and `map2`.
    */
  def map_again[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_again[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) {
      a =>
        flatMap(rb) {
          b => unit(f(a, b))
        }
    }

  /**
    * Exercise 6.8: Implement `flatMap`, and then use it to implement `nonNegativeLessThan`.
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, next) = f(rng)
      g(a)(next)
    }

  def rollDie: Rand[Int] = map_again(nonNegativeLessThan(6))(_ + 1)
}


