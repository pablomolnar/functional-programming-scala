package exercises

import org.scalatest._
import Functions._

class ExercisesTest extends FlatSpec {
  
  it should "salute" in {
    hello() == "Hello World!"
  }
  
  it should "sum a list of numbers recursively" in {
    sum(List(1,2,3,4,5)) == 15 
  }
  
  it should "return the largest element in a list of integers" in {
    max(List(1,20,-3,4,15)) == 20
  }
  
  it should "calculate the correct column and row of the Pascal triangle" in {
    assert(pascal(0,2) === 1)
    assert(pascal(1,2) === 2)
    assert(pascal(1,3) === 3)
  }
  
  it should "verify the correct balance of the following strings" in {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    assert(!balance(":-)".toList))
    assert(!balance("())(".toList))
  }
  
  it should "count the correct number of how many different ways you can make change for an amount and a given list of coin denominations" in {
    assert(countChange(4, List(1,2)) == 3)
    assert(countChange(100, List(1,5,10,25)) == 242)
    assert(countChange(100, List(5,10,25,50)) == 40)
  }
  
}