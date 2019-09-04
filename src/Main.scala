object Main extends App {
  val xl = 4;
  val Xl = 4;
  val yl = 4;
  val defaultNeighbour = List((0,null),(0,null),(0,null),(0,null))
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
      val neighbour = this.neighbour.filter(in._2.x != _._2.x).filter(in._2.y !=_._2.y) :+ in;
      return new Square(this.x,this.y,this.values,this.solved,neighbour);
    }

    def removeValues(in:List[Int]):Square = {
      val s = values.filter(!in.contains(_))
      if(s.length ==1){
        return new Square(this.x,this.y,s,true,this.neighbour :+ neighbour(0));
      }else{
        return new Square(this.x,this.y,s,false,this.neighbour :+ neighbour(0));
      }
    }

    def setValues(in:List[Int]):Square = {
      val s = values.filter(in.contains(_))
      if(s.length ==1){
        return new Square(this.x,this.y,s,true,this.neighbour :+ neighbour(0));
      }else{
        return new Square(this.x,this.y,s,false,this.neighbour :+ neighbour(0));
      }
    }

    def removeValue(in:Int):Square = {
      val solution = this.values.filter(_!=in);
      if(solution.length == 1){
        return new Square(this.x,this.y,solution,true,this.neighbour :+ neighbour(0));
      }else{
        return new Square(this.x,this.y,solution,false,this.neighbour :+ neighbour(0));
      }
    }

    def setValue(solution:Int):Square = {
      val l = List(solution);
      return new Square(this.x,this.y,l,true,this.neighbour);
    }

    def getNeighbours():List[Square] = {
      neighbour.foldRight(List[Square]())(_._2::_);
    }
    
    override def toString() = {
      "x" + x + " y" + y + " " + values.mkString(",") + " " + "\n"
    }
  }

  def getSquareXY(x:Int, y:Int):Square = {
    if(x > xl || y > yl || x ==0 || y ==0) return null
    val s = allSquare.filter(_.x==x).filter(_.y==y)(0);
    return s;
  }

  def removeValue(x:Int,y:Int,sol:Int)={
    var s = getSquareXY(x,y);
    allSquare = allSquare.filter(_!=s);
    s = s.removeValue(sol);
    allSquare = allSquare :+s;
  }

  def removeValues(x:Int,y:Int,sol:List[Int])={
    var s = getSquareXY(x,y);
    allSquare = allSquare.filter(_!=s);
    s = s.removeValues(sol);
    allSquare = allSquare :+s;
  }

  def setValue(x:Int,y:Int,sol:Int):Unit={
    var s = getSquareXY(x,y)
    allSquare = allSquare.filter(_!=s);
    s = s.setValue(sol);
    allSquare = allSquare :+s;
  }

  def setValues(x:Int,y:Int,sol:List[Int]):Unit={
    var s = getSquareXY(x,y)
    allSquare = allSquare.filter(_!=s);
    s = s.setValues(sol);
    allSquare = allSquare :+s;
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
    ret;
  }

  def getNeighbourNotValue(s:Int):List[Int] = {
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

  def printSolution() = {
    var output:String = ""
    for(y<-List(1,2,3,4)){
      for(x<- List(1,2,3,4)){
        var s = getSquareXY(x,y);
        if(s.values.length == 1) output += s.values(0) + "|"
        else {output +=  s.values+ "|"}
      }
      output+="\n"
    }
    print(output)
  }
  var allSquare = List[Square]();
  for (x <- List(1,2,3,4)){
    for(y<-List(1,2,3,4)){
      val s = new Square(x,y);
      allSquare = allSquare :+ s;
    }
  }

  for(s<-allSquare){
    val l = List((s.x+1,s.y),(s.x-1,s.y),(s.x,s.y-1),(s.x,s.y+1));
    for(co<-l){
      val ts = getSquareXY(co._1,co._2);
      if(ts!= null) setNeighbour(s.x,s.y,(1,ts));
    }
  }

  //var neighbours = List[Tuple2]();
  var neighbours = List(((1,1),(1,2)),((1,1),(2,1)),((4,1),(4,2)),((4,1),(3,1)),((2,2),(3,2)));
  //neighbours = neighbours :+ ((2,2),(3,2));
  neighbours = neighbours :+ ((2,2),(2,3));
  neighbours = neighbours :+ ((1,3),(2,3));
  neighbours = neighbours :+ ((1,4),(1,3));
  neighbours = neighbours :+ ((2,4),(3,4));
  neighbours = neighbours :+ ((3,2),(3,3));
  neighbours = neighbours :+ ((3,3),(4,3));
  neighbours = neighbours :+ ((4,4),(4,3));

  for(s <- neighbours){;
    val s1 = getSquareXY(s._1._1,s._1._2);
    val s2 = getSquareXY(s._2._1,s._2._2);

    updateNeighbour(s1.x,s1.y,(2,s2));
    updateNeighbour(s2.x,s2.y,(2,s1));
  }

  def solved():Boolean = {
    if(allSquare.exists(_.solved==false)) false
    true;
  }
  while(!solved()){
    for (x <- List(1,2,3,4)){
      for(y<-List(1,2,3,4)){

      }
    }
  }
  setValue(2,1,1);
  setValue(3,4,3);

  println(getSquareXY(2,1).getNeighbours())
  println(getSquareXY(3,1).neighbour)
  printSolution();
/*
  println(getNeighbourPossibleValues(2));
  println(getNeighbourNotValue(2));

  val s1 = getSquareXY(1,1);
  val s2 = getSquareXY(2,1);
  val v1 = getNeighbourPossibleValues(s2.values(0));
  setValues(1,1,v1);
  println(getSquareXY(1,1).values)

  val s3 = getSquareXY(1,1);
  val v2 = getNeighbourPossibleValues(s3.values(0))
  val v3 = getNeighbourNotValue(s3.values(0))


  println(v2);
  println(v3)
  printSolution();

  removeValues(2,2,getNeighbourNotValue(1));
  println(getSquareXY(2,2).values)
  //println(allSquare)*/
}
