package greeter

object sheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(75); 
  println("Welcome to the Scala worksheet");$skip(17); 
  
    val x = 5;System.out.println("""x  : Int = """ + $show(x ));$skip(31); 
  def increase(i: Int) = i + 1;System.out.println("""increase: (i: Int)Int""");$skip(14); val res$0 = 
  increase(x);System.out.println("""res0: Int = """ + $show(res$0));$skip(37); 
  
  def multiple(x:Int)(y:Int) =x*y;System.out.println("""multiple: (x: Int)(y: Int)Int""");$skip(18); val res$1 = 
  multiple(3)(34);System.out.println("""res1: Int = """ + $show(res$1));$skip(90); 
  
  
  
  lazy val fs: Stream[Int] =
  0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2);System.out.println("""fs: => Stream[Int]""");$skip(310); 


/**

 the prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

get a list of all prime factors
*/

def factors(n: Long): List[Long] =
  (2 to math.sqrt(n).toInt).find(n % _ == 0)
    .map(    i => i.toLong :: factors(n / i)).getOrElse(List(n)         );System.out.println("""factors: (n: Long)List[Long]""");$skip(59); 


val r = factors(600851475143L).last;System.out.println("""r  : Long = """ + $show(r ));$skip(295);  // last of the sieve



     def primesUnder(n: Int): List[Int] = {
        require(n >= 2)

      def rec(i: Int, primes: List[Int]): List[Int] = {
          if (i >= n) primes
          else if (prime(i, primes)) rec(i + 1, i :: primes)
             else rec(i + 1, primes)
          }

  rec(2, List()).reverse
};System.out.println("""primesUnder: (n: Int)List[Int]""");$skip(81); 

def prime(num: Int, factors: List[Int]): Boolean = factors.forall(num % _ != 0);System.out.println("""prime: (num: Int, factors: List[Int])Boolean""");$skip(39); 
//////////////

val numbers = 1 to 100;System.out.println("""numbers  : scala.collection.immutable.Range.Inclusive = """ + $show(numbers ));$skip(27); 
def square(n: Int) = n * n;System.out.println("""square: (n: Int)Int""");$skip(56); 
val ret = square(numbers.sum) - numbers.map(square).sum;System.out.println("""ret  : Int = """ + $show(ret ));$skip(86); 

///////////////////
val res = fs.view.takeWhile(_ <= 4000000).filter(_ % 2 == 0).sum;System.out.println("""res  : Int = """ + $show(res ))}

}
