package greeter
/**
 * In order for a recursive call to be tail recursive, the call back
 *  to the function must be the last action performed in the function. 
 *  In the example above, because the total returned from the recursive 
 *  call is being multiplied by number,
 *  the recursive call is NOT the last action performed in the function, 
 *  so the recursive call is NOT tail recursive.
 */
object Hello extends App {
	println("Hello, World!")


	// this is NOT tail recursive
	def factorial(number:Int) : Int = {
		if (number == 1)
			return 1
					number * factorial (number - 1)
	}


	/**A tail recursive function call allows the compiler to perform a  optimization which it 
	 * normally can not with regular recursion. In a tail recursive function, the recursive call is 
	 * the very last thing to be executed. In this case, instead of allocating a stack frame for each 
	 * call, the compiler can rework the code to *********
	 * simply REUSE the current stack frame, meaning a
	 *  tail-recursive function will only use a single stack frame as opposed to hundreds or even thousands.
	 * 
	 * 
	 */

	// THIS IS tail recursie
	def factorial(accumulator: Int, number: Int) : Int = {
		if(number == 1)  	return accumulator
					else 	factorial(number * accumulator, number - 1)
	}
	//println(factorial(1,8))
  
  def myfac(cur: Int, count: Int): Int= {
   if(count==1) return cur
   else myfac(count*cur,count-1)
    
  }
  println("my")
println(myfac(1, 8));

	/**  Project Euler 31
	 * In England the currency is made up of pound, £, and pence, 
	 * p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
	 * 
	 */
	object MatchTest1 extends App {
		def matchTest(x: Int): String = x match {
		case 1 => "one"
		case 2 => "two"
		case _ => "many"
		}
		//	println(matchTest(1));
	}


	def f(ms: List[Int], n: Int): Int = ms match {
	case h :: t =>
	if (h > n) 0 else if (n == h) 1 else f(ms, n - h) + f(t, n)
	case _ => 0
	} 
	//println(    f(List(1, 2, 5, 10, 20, 50, 100, 200), 200)  );

	//	println(factorial(5))
}

