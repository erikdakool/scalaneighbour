object Square extends App {

  val defaultNeighbour = List((1,square1),(0,null),(0,null),(0,null));
  class Square(val x:Int,val y:Int, val values:List[Int] = List(1,2,3,4), val solved:Boolean = false,val neighbour:List[(Int,Square)]=defaultNeighbour)
  {

  }

}
