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

get a list of all prime factors
*/

def factors(n: Long): List[Long] =
  (2 to math.sqrt(n).toInt).find(n % _ == 0)
    .map(    i => i.toLong :: factors(n / i)).getOrElse(List(n)         )
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
//////////////

val numbers = 1 to 100                            //> numbers  : scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5
                                                  //| , 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 2
                                                  //| 5, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
                                                  //| 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,
                                                  //|  63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81
                                                  //| , 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 1
                                                  //| 00)
def square(n: Int) = n * n                        //> square: (n: Int)Int
val ret = square(numbers.sum) - numbers.map(square).sum
                                                  //> ret  : Int = 25164150

///////////////////
val res = fs.view.takeWhile(_ <= 4000000).filter(_ % 2 == 0).sum
                                                  //> res  : Int = 4613732

}