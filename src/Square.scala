object Square {
  val defaultNeighbour = List((0, null), (0, null), (0, null), (0, null))

  class Square(val x: Int, val y: Int, val values: List[Int] = List(1, 2, 3, 4), val solved: Boolean = false, val neighbour: List[(Int, Square)] = defaultNeighbour) {
    def setValue(value: Int): Square = {
      val list = List(value);
      return new Square(x, y, list, solved = true, neighbour);
    }
  }
}
