package ex6

/**
  * Exercise 6.11: To gain experience with the use of `State`, implement a finite
  * state automaton that models a simple candy dispenser. The machine has two types
  * of input: you can insert a coin, or you can turn the knob to dispense candy. It
  * can be in one of two states: locked or unlocked. It also tracks how many candies
  * are left and how many coins it contains.
  *
  * The rules of the machine are as follows:
  *   - Inserting a coin into a locked machine will cause it to unlock if there's
  *     any candy left.
  *   - Turning the knob on an unlocked machine will cause it to dispense candy
  *     and become locked.
  *   - Turning the knob on a locked machine or inserting a coin into an unlocked
  *     machine does nothing.
  *   - A machine that's out of candy ignores all inputs.
  *
  * The method `simulateMachine` should operate the machine based on the list of
  * inputs and returns the number of coins and candies left in the machine at the
  * end. For example, if the input `Machine` has 10 coins and 5 candies, and a
  * total of 4 candies are successfully bought, the output should be `(14, 1)`
  */

import ex6.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def next(input: Input): Machine = (input, this) match {
    case (_, Machine(_, 0, _)) => this
    case (Coin, Machine(true, _, coins)) => copy(locked = false, coins = coins + 1)
    case (Turn, Machine(false, candies, _)) => copy(locked = true, candies = candies - 1)
    case (Turn, Machine(true, _, _)) => this
    case (Coin, Machine(false, _, _)) => this
  }
}

object Automaton {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    // map over inputs & for each, run modify
    val states: List[State[Machine, Unit]] = inputs.map {
      i => modify[Machine](m => m.next(i))
    }

    for {
      // Flip List[State[Machine, Unit]] <--> State[Machine, List[Unit]]
      _ <- sequence(states)

      // Get the machine from state
      machine <- get
    } yield (machine.coins, machine.candies)
  }
}