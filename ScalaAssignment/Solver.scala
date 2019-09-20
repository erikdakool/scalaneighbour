object Solver {
  var allSquare = List[Square]();
  var Xl = 0;
  def SolveSquare(l:List[Square],x:Int): List[Square] ={
      allSquare = l;
      Xl = x;

    while(allSquare.exists(s=>(!s.solved))) {
      for (x <- List.range(1,Xl+1)){
        for(y <-List.range(1,Xl+1)){
          val s = getSquareXY(x,y);
            for(n<-s.neighbour){
              if(!n._2.solved){
/*                println("=============")
                println(s.x + " " + s.y + "values " + s.values + " is setting at " + n._2.x + " " + n._2.y + "   nei" + getNeighbourPossibleValues(s.values) + "   nnei" + getNotNeighbourPossibleValues((s.values)))
                println("=============") */
                SolveSquare(s.values,n._2,n._1)
              };
            }
        }
      }
      println("=============")
      println("loop complete")
      println("=============")
      printSolution();
    }
    return allSquare;
  }

  def printSolution() = {
    var output:String = ""
    for (y <- List.range(1,Xl+1)){
      for(x <-List.range(1,Xl+1)){
        val s = getSquareXY(x,y);
        if(s.values.length == 1) output += s.values(0) + "|"
        else {output +=   "x|"}
      }
      output+="\n"
    }
    print(output)
  }

  def SolveSquare(i:List[Int],s:Square,t:Int): Unit ={
    if(!s.solved){
      val l = getValuesFromY(s.y);
      if(l.nonEmpty){
        removeValues(s.x,s.y,l);
      }
    }
    if(!s.solved){
      val l = getValuesFromX(s.x);
      if(l.nonEmpty){
        removeValues(s.x,s.y,l);
      }
    }

    if(!s.solved) {
      if(t==1){
        setValues(s.x,s.y,getNotNeighbourPossibleValues(i));
      }else if (t==2){
        setValues(s.x,s.y,getNeighbourPossibleValues(i));
      }
    }
  }

  def getSquareXY(x:Int, y:Int):Square = {
    if(x > Xl || y > Xl || x ==0 || y ==0) return null
    val s = allSquare.filter(_.x==x).filter(_.y==y)(0);
    return s;
  }

  def removeValues(x:Int,y:Int,sol:List[Int])={
    var s = getSquareXY(x,y);
    allSquare = allSquare.filter(_!=s);
    s = s.removeValues(sol);
    allSquare = allSquare :+s;
    //println("Removed values " +sol + " at " + x + " " +y)
  }

  def setValues(x:Int,y:Int,sol:List[Int]):Unit={
    var s = getSquareXY(x,y)
    allSquare = allSquare.filter(_!=s);
    s = s.setValues(sol);
    allSquare = allSquare :+s;
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

    proofValue((higherNeighbour ::: lowerNeighbour).distinct)
    //}
  }

  def getNotNeighbourPossibleValues(s: List[Int]): List[Int] = {
    s.size match {
      case 1 => {
        val n = List(s.head - 1, s.head, s.head + 1);
        proofValue(List.range(1, Xl + 1).filterNot(n.contains(_)));
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

  def proofValue(l:List[Int]):List[Int] = {
    l.filter(_<=Xl).filter(_>=1);
  }

  def getAllX(x:Int):List[Square] = {
    allSquare.filter(_.x == x);
  }

  def getAllY(y:Int):List[Square] = {
    allSquare.filter(_.y==y);
  }

  def getValuesFromX(x:Int):List[Int] ={
    val allX = getAllX(x);
    val s = allX.filter(x=>(x.solved));
    proofValue(List.range(1,Xl+1).filter(x=>(s.exists(s=>s.values(0)==x))));
  }

  def getValuesFromY(y:Int):List[Int] ={
    val allY = getAllY(y);
    val s = allY.filter(x=>(x.solved));
    proofValue(List.range(1,Xl+1).filter(x=>(s.exists(s=>s.values(0)==x))));
  }
}
