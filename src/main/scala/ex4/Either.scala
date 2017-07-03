package ex4

import ex3._

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
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

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