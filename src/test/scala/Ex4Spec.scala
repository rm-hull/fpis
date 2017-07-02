import org.scalatest._


class Ex4Spec extends FunSpec with BeforeAndAfter {

  describe("Ex4.1") {

    it("should map an optional value") {
      assert(Some(3).map(_ + 3) === Some(6))
      assert(None.map((_: Int) + 3) === None)
    }

    it("should flatMap an optional value") {
      assert(Some(3).flatMap(x => Some(x + 3)) === Some(6))
      assert(Some(3).flatMap(x => None) === None)
      assert(None.flatMap((x: Int) => Some(x + 3)) === None)
    }

    it("should getOrElse an optional value") {
      assert(Some(3).getOrElse(7) === 3)
      assert(None.getOrElse(7) === 7)
    }

    it("should orElse an optional value") {
      assert(Some(3).orElse(Some(7)) === Some(3))
      assert(None.orElse(Some(6)) === Some(6))
      assert(None.orElse(None) === None)
    }

    it("should filter an optional value") {
      assert(Some(2).filter(_ % 2 == 0) === Some(2))
      assert(Some(3).filter(_ % 2 == 0) === None)
      assert(None.filter((_: Int) % 2 == 0) === None)
    }
  }

  describe("Ex4.2") {
    import Option._

    it("should calculate mean") {
      assert(Option.mean(Seq.empty[Double]) === None)
      assert(mean(Seq(12, 3, 4, 5, 6)) === Some(6))
    }

    it("should calculate variance") {
      assert(variance(Seq.empty[Double]) === None)
      assert(variance(Seq(12, 3, 4, 5, 6)) == Some(10))
    }
  }

  describe("Ex4.3") {
    import Option._

    def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int) =
      (500 / age) + numberOfSpeedingTickets * 100;

    def parseInsuranceQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
      val optAge = Try {
        age.toInt
      }
      val optTickets = Try {
        numberOfSpeedingTickets.toInt
      }
      map2(optAge, optTickets)(insuranceRateQuote)
    }

    it("should calc insurance quote") {
      assert(parseInsuranceQuote("37", "0") === Some(13.0))
      assert(parseInsuranceQuote("old", "0") === None)
      assert(parseInsuranceQuote("21", "zilch") === None)
    }
  }

  describe("Ex4.4") {
    import Option._

    it("should return None if any input is None") {
      assert(sequence(List(Some(3), Some(4), None)) === None)
    }
    it("should return some list if no input is None") {
      assert(sequence(List(Some(3), Some(4), Some(11))) === Some(List(3, 4, 11)))
    }
    it("should return some list if no input is empty") {
      assert(sequence(List()) === Some(List()))
    }
  }

  describe("Ex4.5") {
    import Option._

    def parseInts(a: List[String]): Option[List[Int]] =
      traverse(a)(i => Try(i.toInt))

    it("should parse ints successfully") {
      assert(parseInts(List("1", "4", "23")) === Some(List(1, 4, 23)))
    }

    it("should return None for parse failure") {
      assert(parseInts(List("1", "4", "Jeff")) === None)
    }

    it("should return None if any input is None") {
      assert(sequence2(List(Some(3), Some(4), None)) === None)
    }
    it("should return some list if no input is None") {
      assert(sequence2(List(Some(3), Some(4), Some(11))) === Some(List(3, 4, 11)))
    }
    it("should return some list if input is empty") {
      assert(sequence2(List()) === Some(List()))
    }
  }

  describe("Ex4.6") {

    it("should map an either value") {
      assert(Left("Oops!").map((_: Int) + 3) === Left("Oops!"))
      assert(Right(3).map(_ + 3) === Right(6))
    }

    it("should flatMap an either value") {
      assert(Left("Oops!").flatMap((x: Int) => Right(x + 3)) === Left("Oops!"))
      assert(Left("Oops!").flatMap((x: Int) => Left("OMG")) === Left("Oops!"))
      assert(Right(6).flatMap((x: Int) => Right(x + 3)) === Right(9))
      assert(Right(6).flatMap((x: Int) => Left("OMG")) === Left("OMG"))
    }

    it("should orElse an either value") {
      assert(Left("Ooops!").orElse(Left("OMG")) === Left("OMG"))
      assert(Left("Ooops!").orElse(Right(3)) === Right(3))
      assert(Right(6).orElse(Right(3)) === Right(6))
      assert(Right(6).orElse(Left("OMG")) === Right(6))
    }

    it("should map2 an either value") {
      def add(a: Int, b: Int) = a + b

      assert(Left("Oops!").map2(Left("OMG"))(add) === Left("Oops!"))
      assert(Right(6).map2(Left("OMG"))(add) === Left("OMG"))
      assert(Left("Oops!").map2(Right(3))(add) === Left("Oops!"))
      assert(Right(6).map2(Right(3))(add) === Right(9))
    }
  }

  describe("Ex4.7") {
    import Either._

    def parseInts(a: List[String]): Either[Exception, List[Int]] =
      traverse(a)(i => Try(i.toInt))

    it("should parse ints successfully") {
      assert(parseInts(List("1", "4", "23")) === Right(List(1, 4, 23)))
    }

    it("should return None for parse failure") {
      parseInts(List("1", "4", "Jeff")) match {
        case Left(ex: NumberFormatException) => assert(ex.getMessage() == "For input string: \"Jeff\"")
        case _ => fail("Unexpected")
      }
    }

    it("should return left if any input is left") {
      assert(sequence(List(Right(3), Right(4), Left("1st"), Left("2nd"))) === Left("1st"))
    }
    it("should return right list if no input is left") {
      assert(sequence(List(Right(3), Right(4), Right(11))) === Right(List(3, 4, 11)))
    }
    it("should return right list if input is empty") {
      assert(sequence(Nil) === Right(Nil))
    }
  }
}