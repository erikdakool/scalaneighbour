object Main extends App {
  val xl = 4;
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
      val neighbour = this.neighbour.filter(_._2!=in._2) :+ in;
      return new Square(this.x,this.y,this.values,this.solved,neighbour);
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

  def setValue(x:Int,y:Int,sol:Int)={
    var s = getSquareXY(x,y)
    allSquare = allSquare.filter(_!=s);
    s = s.setValue(sol);
    allSquare = allSquare :+s;
  }

  def setNeighbour(x:Int,y:Int,in:(Int,Square)) = {
    var s = getSquareXY(x,y);
    allSquare = allSquare.filter(_!=s);
    s = s.setNeighbour(in);
    allSquare = allSquare :+s;
  }

  def updateNeighbour(x:Int,y:Int,in:(Int,Square)) ={
    var s = getSquareXY(x,y);
    allSquare = allSquare.filter(_!= s);
    s = s.updateNeighbour(in);
    allSquare = allSquare :+s;
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

  println(allSquare)
}
