import org.scalatest._


class Ex2Spec extends FunSpec with BeforeAndAfter {
  
  describe( "Ex2.1") {
    it("should yield fibonacci sequence") {
      assert((0 to 10).map(n => Ex2.fib(n)) == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
    }
  }
  
  describe("Ex2.2") {
    it("should return true for a sorted list of numbers") {
      assert(Ex2.isSorted(Array(1, 2, 5, 7), (a: Int, b: Int) => a < b))
    }
    
     it("should return false for an unsorted list of numbers") {
      assert(Ex2.isSorted(Array(9, 2, 4, 17), (a: Int, b: Int) => a < b) == false)
    }
  }
  
  describe("Ex2.3") {
    it("should curry a 2-arg function") {
      val add = Ex2.curry((a: Int, b: Int) => a + b)
      val add5 = add(5)
      assert(add5(9) == 14)
      val decr = add(-1)
      assert(decr(87) == 86)
    }
  }
  
  describe("Ex2.4") {
    it("should uncurry a previously curried function") {
      var mul = Ex2.curry((a: Int, b: Int) => a * b)
      assert(mul(3)(7) == Ex2.uncurry(mul)(3, 7))
    }
  }
  
  describe("Ex2.5") {
    it("should compose two functions") {
      var inc = (n: Int) => n + 1
      var dbl = (n: Int) => 2 * n
      var comp = Ex2.compose(inc, dbl)
      assert(comp(16) == 33)
    }
  }
}