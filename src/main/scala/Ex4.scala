import Option.traverse

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

/**
  * Exercise 4.1: Implement functions on `Option`
  */
case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B) = Some(f(get))
  override def flatMap[B](f: A => Option[B]) = f(get)
  override def getOrElse[B >: A](default: => B) = get
  override def orElse[B >: A](ob: => Option[B]) = this
  override def filter(f: A => Boolean) = if (f(get)) this else None
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = this
  override def flatMap[B](f: Nothing => Option[B]) = this
  override def getOrElse[B >: Nothing](default: => B) = default
  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  override def filter(f: Nothing => Boolean) = this
}


object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2: Implement the `variance` function in terms of `flatMap`.
    * If the mean of a sequence is `m`, the variance is the mean of `math.pow(x - m, 2)`
    * for each element x in the sequence.
    */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m =>
      if (xs.isEmpty) None
      else mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  /**
    * Exercise 4.3: Write a generic function `map2` that combines two
    * `Option` values using a binary function. If either `Option` is
    * `None`, then the return value is too.
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(ay => b.map(by => f(ay, by)))

  /**
    * Exercise 4.4: Write a function `sequence` that combines a list
    * of `Option`s into one `Option` containing a list of all the `Some`
    * values in the original list. If the original list contains `None`
    * even once, the result of the function should be `None`; otherwise
    * the result should be `Some` with a list of all the values.
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Cons(first, rest) => first.flatMap(x => sequence(rest).map(r => Cons(x, r)))
  }

  /**
    * Exercise 4.5: Implement `traverse`. It's straightforward to do using `map`
    * and `sequence`, but try for a more efficient implementation that only looks
    * at the list once. In fact ... implement `sequence` in terms of `traverse`
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case Cons(first, rest) => f(first).flatMap(x => traverse(rest)(f).map(r => Cons(x, r)))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

/**
  * Exercise 4.6 implement functions on `Either`.
  */
case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B) = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]) = this
  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]) = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]) = f(value)
  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]) = this
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C) = b.flatMap(x => Right(f(value, x)))
}

object Either {

  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /**
    * Exercise 4.7: Implement `sequence` and `traverse` for `Either`. These
    * should return the first error that's encountered, if there is one.
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case Cons(first, rest) => f(first).flatMap(x => traverse(rest)(f).map(r => Cons(x, r)))
  }
}

