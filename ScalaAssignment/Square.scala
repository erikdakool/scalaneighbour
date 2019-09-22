class Square(val x:Int,val y:Int, val values:List[Int], val solved:Boolean = false,val neighbour:List[(Int,Square)]=List[(Int,Square)]())
{
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
      return this
    }else return new Square(x = this.x,y = this.y,values = s,solved = false,neighbour = this.neighbour);
  }

  def setValues(in:List[Int]):Square = {
    val s = values.filter(in.contains(_))
    if(s.length ==1){
      return new Square(x=this.x,y =this.y,values = s,solved =true,neighbour = this.neighbour);
    }else if(s.isEmpty){
      return this;
    }else return new Square(x = this.x,y = this.y,values = s,solved = false,neighbour = this.neighbour);
  }

  def retValues(in:List[Int]):Square = {
    if(in.length == 1){
      return new Square(x = this.x,y=this.y,values = in, solved = true, neighbour= this.neighbour);
    }else{
      return new Square(x = this.x,y=this.y,values = in, solved = false, neighbour= this.neighbour);
    }
  }

  def getNeighbours():List[Square] = {
    neighbour.foldRight(List[Square]())(_._2::_);
  }

  override def toString() = {
    "x" + x + " y" + y + " " + values.mkString(",") + " " + "\n"
  }
}