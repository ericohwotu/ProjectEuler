/**
  * Created by Administrator on 19/07/2017.
  */
object CakeIcingPuzzle {

  val pieces: List[Boolean] = List.fill(360)(true)
  val turns: List[Int] = List(60)

  val normal = (x: Int, max: Int, min: Int) => x < max && x >= min
  val looper = (x: Int, max: Int, min: Int) => (x < pieces.length - 1 && x >= max) || (x < min && x >= 0)

  def main(args: Array[String]): Unit = println(switch())

  def switch(): Int = {

    def rec(turn: Int, move: Int)(cur: List[Boolean], loc: Int): Int = cur.forall(x => x) match {
      case true if turn != 0 => turn - 1
      case _ =>
        val zippedPieces = cur.zipWithIndex
        (loc + move > pieces.length) match {
          case true =>
            val res = zippedPieces.map {
                case (dat, ind) => if (looper(ind, loc+move, loc)) !dat else dat
            }
            rec(turn + 1, move)(res,loc + move)
          case false =>
            val res = zippedPieces.map {
              case (dat, ind) => if (normal(ind, loc + move, loc)) !dat else dat
            }
            rec(turn + 1, move)(res,loc + move)
        }
    }

    rec(0, 22)(pieces, 0)
  }

}
