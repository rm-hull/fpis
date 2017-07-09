import ex5._
import org.scalatest._

class Ex5Spec extends FunSpec with BeforeAndAfter {

  describe("Ex5.1") {
    it("should convert stream to a list") {
      assert(Stream(3, 5, 6, 7).toList === List(3, 5, 6, 7))
    }
  }

  describe("Ex5.2") {
    it("should take 3 items") {
      assert(Stream(3, 4, 5, 6, 7, 8).take(3).toList === List(3, 4, 5))
      assert(Stream(3, 4).take(3).toList === List(3, 4))
    }
    it("should drop 3 items") {
      assert(Stream(3, 4, 5, 6, 7, 8).drop(3).toList === List(6, 7, 8))
      assert(Stream(3, 4).drop(3).toList === List.empty)
    }
  }

  describe("Ex5.3") {
    it("should take less than 10") {
      assert(Stream(1, 2, 5, 6, 9, 12, 15, 17).takeWhile(_ < 10).toList === List(1, 2, 5, 6, 9))
      assert(Stream(12, 2, 5, 6, 9, 15, 17).takeWhile(_ < 10).toList === List.empty)
      assert(Stream.empty[Int].takeWhile(_ < 10).toList === List.empty)
    }
  }

  describe("Ex5.4") {
    it("should all be even") {
      assert(Stream.empty[Int].forAll(_ % 2 == 0) === true)
      assert(Stream(2, 6, 10, 992).forAll(_ % 2 == 0) === true)
    }

    it("should not all be even") {
      assert(Stream(2, 6, 99, 10, 992).forAll(_ % 2 == 0) === false)
    }
  }

  describe("Ex5.5") {
    it("should take less than 10") {
      assert(Stream(1, 2, 5, 6, 9, 12, 15, 17).takeWhile2(_ < 10).toList === List(1, 2, 5, 6, 9))
      assert(Stream(12, 2, 5, 6, 9, 15, 17).takeWhile2(_ < 10).toList === List.empty)
      assert(Stream.empty[Int].takeWhile2(_ < 10).toList === List.empty)
    }
  }

  describe("Ex5.6") {
    it("should return the first item from the stream") {
      assert(Stream(1, 2, 3, 4).headOption === Some(1))
      assert(Stream.empty[Int].headOption === None)
    }
  }

  describe("Ex5.7") {
    it("should map stream values") {
      assert(Stream(1, 2, 3, 4).map(_ * 3).toList === List(3, 6, 9, 12))
    }

    it("should filter stream values") {
      assert(Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 1).toList === List(1, 3, 5))
    }

    it("should append streams") {
      assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList === List(1, 2, 3, 4, 5, 6))
      assert(Stream(1, 2, 3).append(Stream.empty).toList === List(1, 2, 3))
      assert(Stream.empty[Int].append(Stream(4, 5, 6)).toList === List(4, 5, 6))
    }
  }

  describe("Ex5.8") {
    it("should return a stream of constants") {
      assert(Stream.constant(4).take(7).toList === List(4, 4, 4, 4, 4, 4, 4))
    }
  }

  describe("Ex5.9") {
    it("should return a sequence of incrementing integers") {
      assert(Stream.from(4).take(7).toList === List(4, 5, 6, 7, 8, 9, 10))
    }
  }

  describe("Ex5.10") {
    it("should return a stream of Fibonacci numbers") {
      assert(Stream.fibs.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
    }
  }

  describe("Ex5.12") {
    it("should return a stream of ones") {
      assert(Stream.ones2.take(7).toList === List(1, 1, 1, 1, 1, 1, 1))
    }
    it("should return a stream of constants") {
      assert(Stream.constant2(4).take(7).toList === List(4, 4, 4, 4, 4, 4, 4))
    }
    it("should return a sequence of incrementing integers") {
      assert(Stream.from2(4).take(7).toList === List(4, 5, 6, 7, 8, 9, 10))
    }
    it("should return a stream of Fibonacci numbers") {
      assert(Stream.fibs2.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
    }
  }

  describe("Ex5.13") {
    it("should map stream values") {
      assert(Stream(1, 2, 3, 4).map2(_ * 3).toList === List(3, 6, 9, 12))
    }
    it("should take 3 items") {
      assert(Stream(3, 4, 5, 6, 7, 8).take2(3).toList === List(3, 4, 5))
      assert(Stream(3, 4).take2(3).toList === List(3, 4))
    }
    it("should take less than 10") {
      assert(Stream(1, 2, 5, 6, 9, 12, 15, 17).takeWhile3(_ < 10).toList === List(1, 2, 5, 6, 9))
      assert(Stream(12, 2, 5, 6, 9, 15, 17).takeWhile3(_ < 10).toList === List.empty)
      assert(Stream.empty[Int].takeWhile3(_ < 10).toList === List.empty)
    }
    it("should sum zipWith") {
      assert(Stream(1,2,3).zipWith(Stream(4,5,6))(_ + _).toList === List(5,7,9))
    }
    it("should zip all") {
      assert(Stream(1,2,3).zipAll(Stream(4,5)).toList ===
        List(
          (Some(1),Some(4)),
          (Some(2),Some(5)),
          (Some(3),None)))
    }
  }

  describe("Ex5.14") {
    it("should handle startsWith") {
      assert(Stream(1,2,3).startsWith(Stream(1,2)))
    }
  }

  describe("Ex5.15") {
    it("should produce tails") {
      assert(Stream(1,2,3).tails.toList.map(_.toList) ===
        List(List(1,2,3), List(2,3), List(3), List()))
    }
  }

  describe("Ex5.16") {
    it("should produce intermediate results") {
      assert(Stream(1,2,3).scanRight(0)(_ + _).toList ===
        List(6,5,3,0))
    }
  }
}