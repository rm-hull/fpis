package ex6

import ex6.State._

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  /**
    * Exercise 6.10: Generalize functions `unit`, `map`, `map2`, `flatMap`, and
    * `sequence`. Add them as methods on the `State` case class where possible.
    * Otherwise you should put them in a `State` companion object.
    */
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(state => {
      val (a, next) = run(state)
      f(a).run(next)
    })
  }

  def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- other
    } yield f(a, b)

}

object State {
  //  type State[S, +A] = S => (A, S)

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty)) {
      (f, acc) => f.map2(acc)(_ :: _)
    }

  def unit[S, A](a: A): State[S, A] =
    State(state => (a, state))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}