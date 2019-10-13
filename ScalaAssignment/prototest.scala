import java.io._;
import Solver.SolveSquare;
import SchemaIn.PSquare;
import SchemaIn.PBoard;
import SchemaIn.PBoards;

object prototest extends App {
  var board = PBoard.newBuilder();
  var square = PSquare.newBuilder();

  for (s<-List(1,2,3,4)) {
    for (s <- List(1, 2, 3, 4)) {
      //board.addSquares(s);
    }
  }
  board.addSquares(13244231)
  board.addSquares(34122134)

  var boards = PBoards.newBuilder();
  boards.addBoards(board)

  val out = new FileOutputStream(new File("ScalaAssignment/sample.bin"))
  boards.build().writeTo(out)

  var in = new FileInputStream("ScalaAssignment/sample.bin");
  var inboard = PBoard.parseFrom(in);
  println(inboard);
}
