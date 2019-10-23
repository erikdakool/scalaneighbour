import java.io._
import scala.io.Source
import Solver.SolveSquare
import schema_out.PBoard
import schema_out.PBoards
import SchemaIn.PPuzzle
import SchemaIn.PPuzzles

import scala.io.Source

class PIO {
  val inputdir = "ScalaAssignment/";
  import java.io.File;
  var boards = schema_out.PBoards.newBuilder();

  val dir = new File(inputdir);

  var inBin = new FileInputStream(new File("ScalaAssignment/puzzle_unsolved.bin"))
  var puzzles = PPuzzles.parseFrom(inBin).getPuzzlesList;
  println(puzzles)
  for (f<-puzzles.getPuzzlesList.toArray()){
    solveBoard(f)
  }

  puzzles.forEach(
    solveBoard(_);
  )

  for(f<-dir.listFiles().toList){
    if(f.getName() == "puzzle_unsolved.txt"){
      val lines = scala.io.Source.fromFile(f).mkString.split("\n")
      val out = new FileOutputStream(new File("ScalaAssignment/sample.bin"))
      boards.build().writeTo(out);

      var in = new FileInputStream("ScalaAssignment/sample.bin");
      var inboard = schema_out.PBoards.parseFrom(in);
      println(inboard);
      var inboards = inboard.getBoardsList;
      inboards.forEach(println(_));
    }
  }


  def solveBoard(s:PPuzzle) = {
    val XL = s.getBoardCount
    var allSquares = List[Square]();
    var neighbours : List[((Int,Int),(Int,Int))] = List()


    def getSquareXY(x:Int, y:Int):Square = {
      if(x > XL || y > XL || x ==0 || y ==0) return null
      val s = allSquares.filter(_.x==x).filter(_.y==y)(0);
      return s;
    }

    def setNeighbourXY(x:Int,y:Int,in:(Int,Square)):Unit = {
      var s = getSquareXY(x,y);
      allSquares = allSquares.filter(_!=s);
      s = s.setNeighbour(in);
      allSquares = allSquares :+s;
    }

    def updateNeighbourXY(x:Int,y:Int,in:(Int,Square)):Unit ={
      var s = getSquareXY(x,y);
      allSquares = allSquares.filter(_!= s);
      s = s.updateNeighbour(in);
      allSquares = allSquares :+s;
    }

    for(s<-allSquares){
      val l = List((s.x+1,s.y),(s.x-1,s.y),(s.x,s.y-1),(s.x,s.y+1));
      for(co<-l){
        val ts = getSquareXY(co._1,co._2);
        if(ts!= null) setNeighbourXY(s.x,s.y,(1,ts));
      }
    }

    for(s <- neighbours){;
      val s1 = getSquareXY(s._1._1+1,s._1._2+1);
      val s2 = getSquareXY(s._2._1+1,s._2._2+1);

      updateNeighbourXY(s1.x,s1.y,(2,s2));
      updateNeighbourXY(s2.x,s2.y,(2,s1));
    }

    allSquares = Solver.SolveBoard(allSquares,XL);


    //Create board square
    var board = schema_out.PBoard.newBuilder();
    board.setX(XL);
    var digitarray = "";
    var count = 0;
    for(y<-List.range(1,XL+1)){
      for(x<-List.range(1,XL+1)){
        //One int per value
        //board.addSquares(s.values(0))

        //Int digit array
        var s = getSquareXY(x,y);
        count += 1;
        if(count > 8){
          board.addSquares(digitarray.toInt);
          count = 1;
          digitarray = s.values(0).toString;
        }else{
          digitarray += s.values(0).toString;
        }
      }
    }
    if(digitarray!= "")
      board.addSquares(digitarray.toInt);
    board.build();
  }
}
