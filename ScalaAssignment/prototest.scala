import java.io._;
import Solver.SolveSquare;
import schema_out.PBoard;
import schema_out.PBoards;

object prototest extends App {
  var board4 = PBoard.newBuilder();

  for (s<-List(1,2,3,4)) {
    for (s <- List(1, 2, 3, 4)) {
      //board.addSquares(s);
    }
  }
  board4.setX(4);
  board4.addSquares(13244231)
  board4.addSquares(34122134)

  var board5 = PBoard.newBuilder();
  board5.setX(5);
  board5.addSquares(43521243);
  board5.addSquares(15243151);
  board5.addSquares(24533514);
  board5.addSquares(251234);

  var boards = PBoards.newBuilder();
  boards.addBoards(board4);
  boards.addBoards(board5);

  val out = new FileOutputStream(new File("ScalaAssignment/sample.bin"))
  boards.build().writeTo(out)


  var in = new FileInputStream("ScalaAssignment/sample.bin");
  var inboard = PBoards.parseFrom(in);
  //println(inboard);
  var inboards = inboard.getBoardsList;
  inboards.forEach(println(_));
}
