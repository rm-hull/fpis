import org.scalatest._


class Ex3Spec extends FunSpec with BeforeAndAfter {
  
  describe("Ex3.1") {
    
    it("should yield expected value") {
      val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }
      
      assert(x === 3)
    }
  }
  
  describe("Ex3.2") {
    
    it("should return Nil when empty") {
      assert(List.tail(Nil) === Nil)
    }
    
    it("should drop first element on non-empty list") {
      assert(List.tail(List(1,2,3,4)) === List(2,3,4))
    }
  }
  
  describe("Ex3.3") {
    
    it("should replace the head item on non-empty list") {
      val orig = List(1,2,3,4)
      assert(List.setHead(99, orig) === List(99,2,3,4))
    }
    
    it("should not alter the existing list") {
      val orig = List(1,2,3,4)
      val newList = List.setHead(99, orig)
      assert(orig !== newList)
    }
    
    it("should set the head on an empty list") {
      assert(List.setHead(87, Nil) === List(87))
    }
  }
  
  describe("Ex3.4") {
    
    it("should not drop anything when n=0") {
      assert(List.drop(List(1,2,3),0) === List(1,2,3))
    }
    
    it("should yield Nil when list is exhausted") {
      assert(List.drop(List(1,2,3),7) === Nil)
    }
    
    it("should drop first two elements") {
      assert(List.drop(List(1,2,3),2) === List(3))
    }
    
    it("should implement generalized tail semantics") {
      val xs = List(4,2,1,8,3)
      assert(List.tail(xs) === List.tail_v2(xs))
    }
  }
  
  describe("Ex3.5") {
    def isEven(n: Int) = n % 2 == 0
      
    it("should yield original list when exhausted and nothing matched the predicate") {
      assert(List.dropWhile(List(1,1,3,5,9,13), isEven) === List(1,1,3,5,9,13))
    }
    
    it("should yield partial list when predicate matches") {
      assert(List.dropWhile(List(2,2,4,5,9,13), isEven) === List(5,9,13))
    }
  }

  describe("Ex3.6") {

    it("should yield nil when empty list provided") {
      assert(List.init(List()) === Nil)
    }

    it("should yield all but last element") {
      assert(List.init(List(1,2,3,4)) === List(1,2,3))
    }
  }

  describe("Ex3.8") {
    it("should reconstruct the list") {
      assert(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) === List(1,2,3))
    }
  }

  describe("Ex3.9") {
    it("should count zero for an empty list") {
      assert(List.length(List()) === 0)
    }

    it("should count correctly for a non-empty list") {
      assert(List.length(List(4,3,1,4)) === 4)
    }
  }

  describe("Ex3.10") {
    it("should return zero for empty list") {
      assert(List.sum3(List()) === 0)
    }

    it("should return correct sum for large-ish list") {
      assert(List.sum3(List(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) === 105)
    }
  }

  describe("Ex3.11") {

    it("should return correct product for non-empty list") {
      assert(List.product3(List(1,2,3,4,5)) === 120)
    }

    it("should count zero for an empty list") {
      assert(List.length3(List()) === 0)
    }

    it("should count correctly for a non-empty list") {
      assert(List.length3(List(4,3,1,4)) === 4)
    }
  }

  describe("Ex3.12") {
    it("should reverse a non-empty list") {
      assert(List.reverse(List(1,2,3)) === List(3,2,1))
    }
  }

  describe("Ex3.14") {
    it("should append two lists") {
      assert(List.append(List(1,2,3),List(4,5,6)) === List(1,2,3,4,5,6))
    }
  }

  describe("Ex3.15") {
    it("should flatten lists of lists") {
      assert(List.flatten(List(List(1,2),List(3),List(4,5,6))) === List(1,2,3,4,5,6))
    }
  }

  describe("Ex3.16") {
    it("should add 1 to each element") {
      assert(List.add1(List(1,2,3,4)) === List(2,3,4,5))
    }
  }

  describe("Ex3.17") {
    it("should change doubles -> string for each element") {
      assert(List.dbl2str(List(1.1,2.2,3.3,4.4)) === List("1.1","2.2","3.3","4.4"))
    }
  }

  describe("Ex3.18") {
    it("should add 5 to each element") {
      assert(List.map(List(1,2,3))(_ + 5) === List(6,7,8))
    }
  }

  describe("Ex3.19") {
    it("should filter odd numbers") {
      assert(List.filter(List(1,2,3,4,5,6,7))(_ % 2 == 1) === List(1,3,5,7))
    }
  }

  describe("Ex3.20") {
    it("should flatmap that shit") {
      assert(List.flatMap(List(1,2,3))(i => List(i,i)) === List(1,1,2,2,3,3))
    }
  }

  describe("Ex3.21") {
    it("should filter odd numbers") {
      assert(List.filter2(List(1,2,3,4,5,6,7))(_ % 2 == 1) === List(1,3,5,7))
    }
  }

  describe("Ex3.22") {
    it("should sum pair-wise") {
      assert(List.sumPairs(List(1,2,3),List(4,5,6)) === List(5,7,9))
    }
  }

  describe("Ex3.23") {
    it("should sum zipWith") {
      assert(List.zipWith(List(1,2,3),List(4,5,6))(_ + _) === List(5,7,9))
    }
  }


  describe("Ex3.25") {
    it("should calc the number of tree nodes") {
      assert(Tree.size(Branch(Branch(Leaf(2),Leaf(3)),Leaf(4))) === 5)
    }
  }

  describe("Ex3.26") {
    it("should calc the max value") {
      assert(Tree.maximum(Branch(Branch(Leaf(2),Leaf(3)),Leaf(4))) === 4)
    }
  }

  describe("Ex3.27") {
    it("should calc the max depth") {
      assert(Tree.depth(Branch(Branch(Leaf(2),Branch(Leaf(2),Leaf(3))),Leaf(4))) === 4)
    }
  }

  describe("Ex3.28") {
    it("should map all values") {
      assert(Tree.map(Branch(Branch(Leaf(2),Branch(Leaf(2),Leaf(3))),Leaf(4)), (x: Int) => x * 2) ===
        Branch(Branch(Leaf(4),Branch(Leaf(4),Leaf(6))),Leaf(8)))
    }
  }

  describe("Ex3.29") {
    it("should calc the number of tree nodes") {
      assert(Tree.size2(Branch(Branch(Leaf(2),Leaf(3)),Leaf(4))) === 5)
    }
    it("should calc the max value") {
      assert(Tree.maximum2(Branch(Branch(Leaf(2),Leaf(3)),Leaf(4))) === 4)
    }
    it("should calc the max depth") {
      assert(Tree.depth2(Branch(Branch(Leaf(2),Branch(Leaf(2),Leaf(3))),Leaf(4))) === 4)
    }
    it("should map all values") {
      assert(Tree.map2(Branch(Branch(Leaf(2),Branch(Leaf(2),Leaf(3))),Leaf(4)), (x: Int) => x * 2) ===
        Branch(Branch(Leaf(4), Branch(Leaf(4), Leaf(6))), Leaf(8)))
    }
  }
}