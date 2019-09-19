object Solver {
  var allSquare = List[Square]();
  var Xl = 0;
  def SolveSquare(l:List[Square],x:Int): List[Square] ={
      allSquare = l;
      Xl = x;

    while(allSquare.exists(s=>(!s.solved))) {
      for (x <- List(1,2,3,4)){
        for(y <-List(1,2,3,4)){
          val s = getSquareXY(x,y);
            for(n<-s.neighbour){
              if(!n._2.solved){
                println(s.x + " " + s.y + "values " + s.values + " is setting at " + n._2.x + " " + n._2.y)
                SolveSquare(s.values(0),n._2,n._1)
              };
            }
        }
        println("=============")
        println("loop complete")
        println("=============")
      }
    }
    return allSquare;
  }

  def SolveSquare(i:Int,s:Square,t:Int): Unit ={
    if(!s.solved){
      removeValues(s.x,s.y,getValuesFromY(s.y));
    }
    if(!s.solved){
      removeValues(s.x,s.y,getValuesFromX(s.x));
    }

    if(!s.solved) {
      if(t==1){
        removeValues(s.x,s.y,getNotNeighbourNotValues(i));
        if(!s.solved){
          setValues(s.x,s.y,getNotNeighbourPossibleValues(i));
        }
      }else if (t==2){
        removeValues(s.x,s.y,getNeighbourNotValues(i));
        if(!s.solved){
          setValues(s.x,s.y,getNeighbourPossibleValues(List(i)));
        }
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
    println("Removed values " +sol + " at " + x + " " +y)
  }

  def setValues(x:Int,y:Int,sol:List[Int]):Unit={
    var s = getSquareXY(x,y)
    allSquare = allSquare.filter(_!=s);
    s = s.setValues(sol);
    allSquare = allSquare :+s;
    println("Set values " + sol + " at " + x + " " +y)
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


  def getNeighbourNotValues(s:Int):List[Int] = {
    val n = List(s-1,s+1)
    proofValue(List.range(1,Xl+1).filterNot(n.contains(_)))
  }

  def getNotNeighbourPossibleValues(s:Int):List[Int] = {
    val n = List(s-1,s,s+1);
    proofValue(List.range(1,Xl+1).filterNot(n.contains(_)));
  }


  def getNotNeighbourNotValues(s:Int):List[Int] = {
    proofValue(List(s-1,s,s+1));
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
    proofValue(List.range(1,Xl+1).filter(x=>(s.exists(s=>s.values(0)==x))));  }

  def getValuesFromY(y:Int):List[Int] ={
    val allY = getAllY(y);
    val s = allY.filter(x=>(x.solved));
    proofValue(List.range(1,Xl+1).filter(x=>(s.exists(s=>s.values(0)==x))));
  }
}
