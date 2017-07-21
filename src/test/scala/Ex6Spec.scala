import ex6.RNG._
import ex6._
import org.scalatest._

class Ex6Spec extends FunSpec with BeforeAndAfter {

  case class ConstantRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = {
      (value, ConstantRNG(value))
    }
  }

  case class SequentialRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = {
      (value, SequentialRNG(value + 1))
    }
  }

  case class FromRNG(values: Int*) extends RNG {
    override def nextInt: (Int, RNG) = {
      values match {
        case x :: xs => (x, FromRNG(xs: _*))
        case _ => (-1, ConstantRNG(-1))
      }
    }
  }

  describe("Ex6.1") {
    it("should produce a non negative int") {
      assert(nonNegativeInt(ConstantRNG(32))._1 === 32)
      assert(nonNegativeInt(ConstantRNG(-32))._1 === 31)
      assert(nonNegativeInt(ConstantRNG(0))._1 === 0)
      assert(nonNegativeInt(ConstantRNG(-1))._1 === 0)
      assert(nonNegativeInt(ConstantRNG(Int.MinValue))._1 === Int.MaxValue)
    }
  }

  describe("Ex6.2") {
    it("should produce a double") {
      assert(double(ConstantRNG(0))._1 === 0.0)
      assert(double(ConstantRNG(Int.MaxValue))._1 !== 1.0)
      assert(double(ConstantRNG(5000000))._1 === 0.0023283064365386963)
    }
  }

  describe("Ex6.3") {
    it("should produce an (int, double) pair") {
      assert(intDouble(ConstantRNG(5000000))._1 === (5000000, 0.0023283064365386963))
    }
    it("should produce an (double, int) pair") {
      assert(doubleInt(ConstantRNG(5000000))._1 === (0.0023283064365386963, 5000000))
    }
    it("should produce a (double, double, double) triple") {
      assert(double3(ConstantRNG(5000000))._1 === (0.0023283064365386963, 0.0023283064365386963, 0.0023283064365386963))
    }
  }

  describe("Ex6.4") {
    it("should produce a list of ints") {
      assert(ints(3)(SequentialRNG(12))._1 === List(12, 13, 14))
      assert(ints(0)(SequentialRNG(12))._1 === List())
    }
  }

  describe("Ex6.5") {
    it("should produce a double") {
      assert(double2(ConstantRNG(0))._1 === 0.0)
      assert(double2(ConstantRNG(Int.MaxValue))._1 !== 1.0)
      assert(double2(ConstantRNG(5000000))._1 === 0.0023283064365386963)
    }
  }

  describe("Ex6.6") {
    it("should combine two actions") {
      assert(map2(nonNegativeInt, nonNegativeInt)(_ + _)(SequentialRNG(2))._1 === 5)
    }
  }

  describe("Ex6.7") {
    it("should sequence a list of Rand[A]") {
      assert(sequence(List.fill(5)(nonNegativeInt(_)))(SequentialRNG(2))._1 === List(2, 3, 4, 5, 6))
      assert(sequence2(List.fill(5)(nonNegativeInt(_)))(SequentialRNG(2))._1 === List(2, 3, 4, 5, 6))
    }
  }

  describe("Ex6.8") {
    it("should return a value less than 5") {
      assert(nonNegativeLessThan(5)(SequentialRNG(-9))._1 === 3)
      assert(nonNegativeLessThan2(5)(SequentialRNG(-9))._1 === 3)
    }
  }

  describe("Ex6.9") {
    def double_again: Rand[Double] = {
      map_again(nonNegativeInt)(i => i / (Int.MaxValue + 1.0))
    }

    it("should map again") {
      assert(double_again(ConstantRNG(0))._1 === 0.0)
      assert(double_again(ConstantRNG(Int.MaxValue))._1 !== 1.0)
      assert(double_again(ConstantRNG(5000000))._1 === 0.0023283064365386963)
    }

    it("should map2 again") {
      assert(map2_again(nonNegativeInt, nonNegativeInt)(_ + _)(SequentialRNG(2))._1 === 5)
    }
  }

  describe("Ex6.10") {
    type Rand2[A] = State[RNG, A]

    def nni: Rand2[Int] =
      State(rng => rng.nextInt match {
        case (i, next) if i >= 0 => (i, next)
        case (i, next) => (-(i + 1), next)
      })

    it("should map") {
      assert(State.unit(2).map(_ + 1).run(SequentialRNG(5))._1 === 3)
    }

    it("should map2") {
      assert(State.unit(2).map2(nni)(_ + _).run(ConstantRNG(5))._1 === 7)
    }

    it("should sequence a list of Rand2[A]") {
      assert(State.sequence(List.fill(5)(nni)).run(SequentialRNG(-2))._1 === List(1, 0, 0, 1, 2))
    }
  }

  describe("Ex6.11") {
    it("should buy 4 candies") {
      val machine = Machine(locked = true, coins = 10, candies = 5)
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      assert(Automaton.simulateMachine(inputs).run(machine)._1 === (14, 1))
    }

    it("should not buy anything when empty") {
      val machine = Machine(locked = true, coins = 10, candies = 0)
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      assert(Automaton.simulateMachine(inputs).run(machine)._1 === (10, 0))
    }

    it("should lock if turning knob on unlocked machine") {
      val machine = Machine(locked = false, coins = 10, candies = 5)
      val inputs = List(Turn)
      assert(Automaton.simulateMachine(inputs).run(machine)._2 === Machine(locked = true, 4, 10))
    }

    it("should unlock if inserting coin in locked machine") {
      val machine = Machine(locked = true, coins = 10, candies = 5)
      val inputs = List(Coin)
      assert(Automaton.simulateMachine(inputs).run(machine)._2 === Machine(locked = false, 5, 11))
    }

    it("should do nothing if inserting coin in unlocked machine") {
      val machine = Machine(locked = false, coins = 10, candies = 5)
      val inputs = List(Coin)
      assert(Automaton.simulateMachine(inputs).run(machine)._2 === machine)
    }

    it("should do nothing if turning knob on locked machine") {
      val machine = Machine(locked = true, coins = 10, candies = 5)
      val inputs = List(Turn)
      assert(Automaton.simulateMachine(inputs).run(machine)._2 === machine)
    }

    it("should ignore all imports if out of candy") {
      val machine = Machine(locked = false, coins = 10, candies = 0)
      val inputs = List(Coin, Turn, Turn, Coin)
      assert(Automaton.simulateMachine(inputs).run(machine)._2 === machine)
    }
  }
}