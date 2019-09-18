object Main extends App {
  val xl = 4;
  val Xl = 4;
  val yl = 4;
  val dn = List[(Int,Square)]();
  class Square(val x:Int,val y:Int, val values:List[Int] = List(1,2,3,4), val solved:Boolean = false,val neighbour:List[(Int,Square)]=dn)
  {
    def setNeighbour(x:Int,y:Int): Square ={
      val neighbour = List((2,getSquareXY(x,y)));
      return new Square(this.x,this.y,this.values,this.solved,this.neighbour :+ neighbour(0));
    }

    def setNeighbour(in:(Int,Square)):Square = {
      val neighbour = this.neighbour :+ in;
      return new Square(this.x,this.y,this.values,this.solved,neighbour);
    }

    def updateNeighbour(in:(Int,Square)):Square = {
      val neighbour = this.neighbour.filterNot(s=>(s._2.x == in._2.x && s._2.y == in._2.y)) :+in;
      return new Square(this.x,this.y,this.values,this.solved,neighbour);
    }

    def removeValues(in:List[Int]):Square = {
      val s = values.filter(!in.contains(_))
      if(s.length ==1){
        return new Square(x=this.x,y =this.y,values = s,solved =true,neighbour = this.neighbour);
      }else if (s.isEmpty){
        return new Square(x=this.x,y =this.y,values = this.values,solved =true,neighbour = this.neighbour);
      }else return new Square(x = this.x,y = this.y,values = s,solved = false,neighbour = this.neighbour);
    }

    def setValues(in:List[Int]):Square = {
      val s = values.filter(in.contains(_))
      if(s.length ==1){
        return new Square(x=this.x,y =this.y,values = s,solved =true,neighbour = this.neighbour);
      }else if(s.isEmpty){
        return new Square(x=this.x,y =this.y,values = this.values,solved =true,neighbour = this.neighbour);
      }else return new Square(x = this.x,y = this.y,values = s,solved = false,neighbour = this.neighbour);
    }

    def getNeighbours():List[Square] = {
      neighbour.foldRight(List[Square]())(_._2::_);
    }
    
    override def toString() = {
      "x" + x + " y" + y + " " + values.mkString(",") + " " + "\n"
    }
  }

  var allSquare = List[Square]();
  for (x <- List(1,2,3,4)){
    for(y<-List(1,2,3,4)){
      val s = new Square(x,y);
      allSquare = allSquare :+ s;
    }
  }

  def getSquareXY(x:Int, y:Int):Square = {
    if(x > xl || y > yl || x ==0 || y ==0) return null
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

  def setNeighbour(x:Int,y:Int,in:(Int,Square)):Unit = {
    var s = getSquareXY(x,y);
    allSquare = allSquare.filter(_!=s);
    s = s.setNeighbour(in);
    allSquare = allSquare :+s;
  }

  def updateNeighbour(x:Int,y:Int,in:(Int,Square)):Unit ={
    var s = getSquareXY(x,y);
    allSquare = allSquare.filter(_!= s);
    s = s.updateNeighbour(in);
    allSquare = allSquare :+s;
  }

  //Pattern matching
  def getValueFromLane(l:List[Int]):Int ={
    val l2 = List.range(1,xl+1).filter(!l.contains(_));
    l2(0)
  }

  def getNeighbourPossibleValues(s:Int):List[Int] = {
    val ret = proofValue(s match {
      case 1  => List(2);
      case Xl => List(xl-1);
      case _  => List(s+1,s-1);
    })
    proofValue(ret);
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
    l.filter(_<=xl).filter(_>=1);
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


  def printSolution() = {
    var output:String = ""
    for(y<-List(1,2,3,4)){
      for(x<- List(1,2,3,4)){
        var s = getSquareXY(x,y);
        if(s.values.length == 1) output += s.values(0) + "|"
        else {output +=   "x|"}
      }
      output+="\n"
    }
    print(output)
  }

  for(s<-allSquare){
    val l = List((s.x+1,s.y),(s.x-1,s.y),(s.x,s.y-1),(s.x,s.y+1));
    for(co<-l){
      val ts = getSquareXY(co._1,co._2);
      if(ts!= null) setNeighbour(s.x,s.y,(1,ts));
    }
  }

  var neighbours = List(((1,1),(1,2)));
  neighbours = neighbours :+ ((1,2),(2,2));
  neighbours = neighbours :+ ((1,3),(1,2));
  neighbours = neighbours :+ ((1,4),(1,3));
  neighbours = neighbours :+ ((1,3),(2,3));

  neighbours = neighbours :+ ((2,1),(3,1));
  neighbours = neighbours :+ ((2,4),(3,4));

  neighbours = neighbours :+ ((4,1),(4,2));
  neighbours = neighbours :+ ((3,2),(4,2));
  neighbours = neighbours :+ ((4,3),(4,2));
  neighbours = neighbours :+ ((3,3),(4,3));
  neighbours = neighbours :+ ((4,4),(4,3));

  for(s <- neighbours){;
    val s1 = getSquareXY(s._1._1,s._1._2);
    val s2 = getSquareXY(s._2._1,s._2._2);

    updateNeighbour(s1.x,s1.y,(2,s2));
    updateNeighbour(s2.x,s2.y,(2,s1));
  }

  setValues(1,1,List(1));
  setValues(4,4,List(1));

  printSolution();

  while(allSquare.exists(s=>(!s.solved))) {
    for (x <- List(1,2,3,4)){
      for(y <-List(1,2,3,4)){
        val s = getSquareXY(x,y);
        if(s.solved){
          for(n<-s.neighbour){
            if(!n._2.solved){
              println(s.x + " " + s.y + " is setting at " + n._2.x + " " + n._2.y)
              SolveSquare(s.values(0),n._2,n._1)
            };
          }
        }
      }
      println("=============")
      println("loop complete")
      println("=============")
    }
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
          setValues(s.x,s.y,getNeighbourPossibleValues(i));
        }
      }
    }
  }

  println("-------------")
  printSolution();
}
