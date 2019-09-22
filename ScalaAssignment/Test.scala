object Test extends App {
  var l = List.range(1,5);

  def setL(in:List[Int]):Unit = {
    l = l.filter(in.contains(_))
  }

  setL(List(1,2))
  setL(List(4,2))
  setL(List(4,2,3,1))
  println(l);
}