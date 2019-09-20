object Test extends App {
  val Xl = 5;
  def getNotNeighbourPossibleValues(s: List[Int]): List[Int] = {
    s.size match {
      case 1 => {
        val n = List(s.head - 1, s.head, s.head + 1);
        List.range(1, Xl + 1).filterNot(n.contains(_));
      }
      case 2 => {
        if (s.head == s.last - 1 || s.head == s.last + 1) {
          List.range(1,Xl+1).filter(_!=s.head).filter(_!=s.last);
        }
        else if (s.head == s.last - 2 || s.head == s.last + 2) {
          List.range(1,Xl+1).filter(_!=(s.head + s.last)/2)
        }
        else {
          return List[Int]()
        }
      }
      case 3 => {
        if ((s.head == s(1) + 1 && s.last == s(1) - 1) || (s.head == s(1) - 1 && s.last == s(1) + 1)) {
          List.range(1,Xl+1).filter(_ != s(1))
        }
        else {
          List[Int]()
        }
      }
      case _ => return List[Int]()
    }
  }

  def getNeighbourPossibleValues(s:List[Int]):List[Int] = {
    /*if(s.size==1) {
      val ret = s.head match {
        case 1 => List(2);
        case Xl => List(Xl - 1);
        case _ => List(s.head + 1, s.head - 1);
      }
       proofValue(ret);
    }
    else{*/
    val higherNeighbour:List[Int] = for(x <- s)
      yield x+1

    val lowerNeighbour:List[Int] = for(x <- s)
      yield x-1

    (higherNeighbour ::: lowerNeighbour).distinct
    //}
  }

  println(getNotNeighbourPossibleValues(List(4)))
  println(getNotNeighbourPossibleValues(List(2)))
  println(getNeighbourPossibleValues(List(2)))
}