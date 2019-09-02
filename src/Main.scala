object Main extends App {
  val defaultNeighbour = List((0,null),(0,null),(0,null),(0,null))
  class Square(val x:Int,val y:Int, val values:List[Int] = List(1,2,3,4), val solved:Boolean = false,val neighbour:List[(Int,Square)]=defaultNeighbour)
  {
    def setNeighbour(x:Int,y:Int): Square ={
      val neighbour = List((2,getSquareXY(x,y)));
      return new Square(this.x,this.y,this.values,this.solved,this.neighbour :+ neighbour(0));
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
      val solution = List(solution);
      return new Square(this.x,this.y,solution,true,this.neighbour :+ neighbour(0));
    }
  }

  def getSquareXY(x:Int, y:Int):Square = {
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
  var s = new Square(1,1);

  var allSquare = List[Square]();
  for (x <- List(1,2,3,4)){
    for(y<-List(1,2,3,4)){
      val s = new Square(x,y);
      allSquare = allSquare :+ s;
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
    var sq = getSquareXY(s._1._1,s._1._2)
    if(sq.neighbour.length >=4){
      allSquare = allSquare.filter(_!=sq);
      sq =  sq.setNeighbour(s._2._1,s._2._2);
      allSquare = allSquare :+ sq;

      sq = getSquareXY(s._2._1,s._2._2);
      allSquare = allSquare.filter(_!=sq);
      sq = sq.setNeighbour(s._1._1,s._1._2);
      allSquare = allSquare :+ sq;
    }
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
