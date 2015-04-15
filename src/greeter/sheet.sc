package greeter

object sheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
    val x = 5                                     //> x  : Int = 5
  def increase(i: Int) = i + 1                    //> increase: (i: Int)Int
  increase(x)                                     //> res0: Int = 6
  
  def multiple(x:Int)(y:Int) =x*y                 //> multiple: (x: Int)(y: Int)Int
  multiple(3)(34)                                 //> res1: Int = 102
  
  
  
  lazy val fs: Stream[Int] =
  0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2)
                                                  //> fs: => Stream[Int]


/**

 the prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*/

def factors(n: Long): List[Long] =
  (2 to math.sqrt(n).toInt).find(n % _ == 0)
    .map(i => i.toLong :: factors(n / i)).getOrElse(List(n))
                                                  //> factors: (n: Long)List[Long]

val r = factors(600851475143L).last // last of the sieve
                                                  //> r  : Long = 6857



     def primesUnder(n: Int): List[Int] = {
        require(n >= 2)

      def rec(i: Int, primes: List[Int]): List[Int] = {
          if (i >= n) primes
          else if (prime(i, primes)) rec(i + 1, i :: primes)
             else rec(i + 1, primes)
          }

  rec(2, List()).reverse
}                                                 //> primesUnder: (n: Int)List[Int]

def prime(num: Int, factors: List[Int]): Boolean = factors.forall(num % _ != 0)
                                                  //> prime: (num: Int, factors: List[Int])Boolean



val res = fs.view.takeWhile(_ <= 4000000).filter(_ % 2 == 0).sum
                                                  //> res  : Int = 4613732

}