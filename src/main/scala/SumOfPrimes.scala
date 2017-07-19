/**
  * Created by Administrator on 19/07/2017.
  */
object SumOfPrimes extends App{

  println(sumPrimes(100))

  def sumPrimes(limit: Int): Int ={

    def rec(num: Int, primes: List[Int])(sum: Int, res: List[Int]): Int = (primes.forall( num % _ != 0 )) match{

      case _ if res.sum + num >= limit => res.sum

      case true if primes.forall((res.sum + num) % _ !=0 ) =>
        val newPrime = primes :+ num
        rec(num+1, newPrime)(newPrime.sum, res :+ num)
      case _ => rec(num+1, primes)(sum,res)
    }

    rec(5, List(2,3,5))(0,List(2))
  }

}
