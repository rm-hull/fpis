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
      
      assert(x == 3)
    }
  }
  
  describe("Ex3.2") {
    
    it("should return Nil when empty") {
      assert(List.tail(Nil) == Nil)
    }
    
    it("should drop first element on non-empty list") {
      assert(List.tail(List(1,2,3,4)) == List(2,3,4))
    }
  }
  
  describe("Ex3.3") {
    
    it("should replace the head item on non-empty list") {
      val orig = List(1,2,3,4)
      assert(List.setHead(99, orig) == List(99,2,3,4))
    }
    
    it("should not alter the existing list") {
      val orig = List(1,2,3,4)
      val newList = List.setHead(99, orig)
      assert(orig != newList)
    }
    
    it("should set the head on an empty list") {
      assert(List.setHead(87, Nil) == List(87))
    }
  }
  
  describe("Ex3.4") {
    
    it("should not drop anything when n=0") {
      assert(List.drop(List(1,2,3),0) == List(1,2,3))
    }
    
    it("should yield Nil when list is exhausted") {
      assert(List.drop(List(1,2,3),7) == Nil)
    }
    
    it("should drop first two elements") {
      assert(List.drop(List(1,2,3),2) == List(3))
    }
    
    it("should implement generalized tail semantics") {
      val xs = List(4,2,1,8,3)
      assert(List.tail(xs) == List.tail_v2(xs))
    }
  }
  
  describe("Ex3.5") {
    def isEven(n: Int) = n % 2 == 0
      
    it("should yield original list when exhaused and nothing matched the predicate") {
      assert(List.dropWhile(List(1,1,3,5,9,13), isEven) == List(1,1,3,5,9,13))
    }
    
    it("should yield partial list when predicate matches") {
      assert(List.dropWhile(List(2,2,4,5,9,13), isEven) == List(5,9,13))
    }
  }
}