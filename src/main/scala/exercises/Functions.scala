package exercises

object Functions {
  /**
   *  Hello World
   */ 
  def hello(): String = "Hello World!"
  
  /**
   *  Compute the sum of all number of a List
   */ 
  def sum(xs: List[Int]): Int = {
    def loop(acc: Int, xs:List[Int]): Int = {
      if(xs.isEmpty) acc 
      else loop(acc + xs.head, xs.tail)
    }
    
	loop(0, xs)
  }
  
  /**
   * Calculate the largest number of a List
   */
  def max(xs: List[Int]): Int = {
    def loop(max: Int, xs:List[Int]): Int = { 
      if(xs.isEmpty) max 
      else if(max > xs.head) loop(max, xs.tail) else loop(xs.head, xs.tail)
    }
      
	loop(xs.head, xs.tail)
  }
  
  /**
   * Pascal's Triangle
   * 
   * The following pattern of numbers is called Pascal's triangle.
   * The numbers at the edge of the triangle are all 1, and each number inside the triangle 
   * is the sum of the two numbers above it. Write a function that computes the elements of 
   * Pascal's triangle by means of a recursive process.
   * 
   *	    1
   *	   1 1
   *	  1 2 1
   *	 1 3 3 1
   *	1 4 6 4 1
   *	   ...
   *    
   *    
   *  c: column
   *  r: row 
   */
  def pascal(c: Int, r: Int): Int = {
  	if(c < 0 || c > r) return 0
  	if(c == 0 || r == 0) return 1
  	
  	pascal(c-1,r-1) + pascal(c,r-1)
  }   

  /**
   * Parentheses Balancing
   * Recursive function which verifies the balancing of parentheses in a string
   */
  def balance(chars: List[Char]): Boolean = {
  	def loop(chars: List[Char], balance: Int): Boolean = {
  		if(balance < 0)   return false
  		if(chars.isEmpty) return balance == 0
  	
  		if(chars.head == '(' )      loop(chars.tail, balance + 1)
  		else if(chars.head == ')' ) loop(chars.tail, balance - 1)
  		else                        loop(chars.tail, balance)
  	}
  
  	loop(chars, 0)
  }
  
  /**
   * Counting Change
   * Function that counts how many different ways you can make change for an amount and a given list of coin denominations. 
   * For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   * 
   */
  def countChange(money: Int, coins: List[Int]): Int = {
  
    def loop(money: Int, coins: List[Int], count: Int): Int = {
      if (coins.isEmpty || money < 0) return 0
      if (money == 0)  return count + 1

      loop(money, coins.tail, count) + loop(money - coins.head, coins, count)
    }

    loop(money, coins, 0)
  }  
  
  
}