/**
  * Created by Administrator on 19/07/2017.
  */
object Palindrome extends App{

  println(base(1000))
  def base(limit: Int): Int ={

    def rec(num: Int)(sum: Int): Int =(isPalidrom(num.toString) && isPalidrom(num.toBinaryString)) match{
      case _ if num==0 => sum
      case true => rec(num-1)(sum + num)
      case false => rec(num-1)(sum)
    }
    rec(limit)(0)
  }

  def isPalidrom(numString: String): Boolean = {

    def rec(list: List[Char]): Boolean = list match {
      case x if (x.length<=1) => true
      case x if (x.headOption.fold(0)(x=>x) != x.lastOption.fold(0)(x=>x)) => false
      case x =>
        val top = x.reverse.drop(1)
        val mid = top.reverse.drop(1)
        rec(mid)
    }
    rec(numString.toList)
  }
}
