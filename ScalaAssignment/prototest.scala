import java.io._;
import Solver.SolveSquare;
import SchemaIn.PSquare;
import SchemaIn.PBoard;
import SchemaIn.PBoards;

object prototest extends App {
  var board = PBoard.newBuilder();
  var square = PSquare.newBuilder();
  board.addSquares(square);
  board.setX(4);
  val out = new FileOutputStream(new File("ScalaAssignment/sample.bin"))
  board.build().writeTo(out)

  var in = new FileInputStream("ScalaAssignment/sample.bin");
  var inboard = PBoard.parseFrom(in);
  println(inboard);
}
