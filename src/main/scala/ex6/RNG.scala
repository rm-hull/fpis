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

  /**
    * Exercise 6.2: Write a function to generate a `Double` between `0` and `1`, not
    * including `1`.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, next) = nonNegativeInt(rng)
    ((i / (Int.MaxValue + 1.0)), next)
  }

  /**
    * Exercise 6.3: Write functions to generate an `(Int, Double)` pair, a `(Double, Int)`
    * pair, and a `(Double, Double, Double)` 3-tuple.
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, next) = rng.nextInt
    val (d, next2) = double(next)
    ((i, d), next2)
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
      case ((lst, rng), _) => {
        val (i, next) = rng.nextInt
        (lst :+ i, next)
      }
    }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /**
    * Exercise 6.5: Use `map` to reimplement `double` in a more elegant way.
    */
  def double2: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue + 1.0))
  }
}


