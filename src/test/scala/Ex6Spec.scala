import ex6._
import RNG._
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
      assert(intDouble(ConstantRNG(5000000))._1 === (5000000,0.0023283064365386963))
    }
    it("should produce an (double, int) pair") {
      assert(doubleInt(ConstantRNG(5000000))._1 === (0.0023283064365386963,5000000))
    }
    it("should produce a (double, double, double) triple") {
      assert(double3(ConstantRNG(5000000))._1 === (0.0023283064365386963,0.0023283064365386963,0.0023283064365386963))
    }
  }

  describe("Ex6.4") {
    it("should produce a list of ints") {
      assert(ints(3)(SequentialRNG(12))._1 === List(12,13,14))
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
}