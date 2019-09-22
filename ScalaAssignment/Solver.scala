object Solver {
  var allSquare = List[Square]();
  var Xl = 0;
  def SolveBoard(l:List[Square],x:Int): List[Square] ={
      allSquare = l;
      Xl = x;

    SolveSquareBrute(getSquareXY(1,1))
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

  def GetPossibleValues(s:Square):List[Int] = {
    val xv = getValuesFromX(s.x);
    val yv = getValuesFromY(s.y);

    GetValuesFrommAllNeighbours(s).filterNot(xv.contains(_)).filterNot(yv.contains(_));
  }

  def SolveSquare(s:Square):Boolean = {
    val l = GetValuesFrommAllNeighbours(s);
    if(l.isEmpty){
      return false;
    }
    setValues(s.x,s.y,l);
    true;
  }

  def SolveSquareBrute(s:Square):Boolean = {
    if(s.solved){
      if(s.x == Xl && s.y == Xl){
        return true;
      }
      SolveSquareBrute(GetNextSquare(s));
    }
    else{
      val oldValues = s.values;
      for(i<-oldValues){
        if(s.x == 2 && s.y == 2 && getSquareXY(1,2).values(0) == 4){
          ValidSquare(List(1),s);
        }
        if(ValidSquare(List(i),s)){
          retValues(s.x,s.y,List(i));
          if(s.x == Xl && s.y == Xl){
            return true;
          }
          if(SolveSquareBrute(GetNextSquare(s))){
            return true;
          }
        }
      }
      retValues(s.x,s.y,oldValues)
      return false;
    }
  }

  def GetNextSquare(s:Square):Square = {
    if(s.x == Xl){
      if(s.y == Xl){
        return getSquareXY(1,1);
      }else{
        return getSquareXY(1,s.y+1)
      }
    }else{
      return getSquareXY(s.x+1,s.y);
    }
  }

  def GetValuesFrommAllNeighbours(s:Square):List[Int] ={
    var l = List.range(1,Xl+1)
    for (a<-s.neighbour) yield {
      val values = GetPossibleValuesFromNeighbour(getSquareXY(a._2.x,a._2.y).values, a._1)
      l = l.filter(values.contains(_))
    }
    return l;
  }

  def GetPossibleValuesFromNeighbour(values:List[Int],t:Int):List[Int] ={
    if(t==1){
      return getNotNeighbourPossibleValues(values);
    }else if(t==2){
      return getNeighbourPossibleValues(values);
    }
    return List.range(1,Xl+1)
  }

  def ValidSquare(l:List[Int],s:Square):Boolean = {
    val xv = getValuesFromX(s.x);
    val yv = getValuesFromY(s.y);
    val nvalues = GetPossibleValues(s).filterNot(xv.contains(_)).filterNot(yv.contains(_));
    l.forall(nvalues.contains)
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

  def retValues(x:Int,y:Int,sol:List[Int]):Unit = {
    var s = getSquareXY(x,y);
    allSquare = allSquare.filter(_!=s);
    s = s.retValues(sol);
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
          return List.range(1,Xl+1)
        }
      }
      case 3 => {
        if ((s.head == s(1) + 1 && s.last == s(1) - 1) || (s.head == s(1) - 1 && s.last == s(1) + 1)) {
          List.range(1,Xl+1).filter(_ != s(1))
        }
        else {
          return List.range(1,Xl+1)
        }
      }
      case _ => return List.range(1,Xl+1)
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
