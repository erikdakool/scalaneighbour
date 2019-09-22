import java.io._;
import Solver.SolveSquare;

class IO {
  val inputdir = "ScalaAssignment/";
  import java.io.File;

  val dir = new File(inputdir);
  for(f<-dir.listFiles()){
    if(f.getName() == "puzzle_unsolved.txt"){
      val lines = scala.io.Source.fromFile(f).mkString.split("\n")
      val output = lines(0) + "\n" + solveNextBoard(lines.takeRight(lines.length-1))
      print(output);

      val out = new PrintWriter(new File(inputdir+ "puzzle_solved.txt"),"UTF-8");
      out.print(output);
      out.close();
    }
  }

  def solveNextBoard(s:Array[String]):String = {
    val XL = s(0).toCharArray()(5).asDigit;
    val solved = solveBoard(s.take(XL*2));
    if(s.length > XL*2){      return solved + solveNextBoard(s.takeRight(s.length-XL*2))
    }else{
      return solved
    }
  }

  def solveBoard(s:Array[String]):String = {
    val lines = s;
    val XL = lines(0).toCharArray()(5).asDigit;

    var allSquares = List[Square]();
    var neighbours : List[((Int,Int),(Int,Int))] = List()
    for(y<- 1 until XL*2){
      val l = lines(y).toCharArray;
      for(x<- 0 until XL*3+2 by 2){
        l(x) match{
          case '_' => {
            val s = new Square(x/4+1,(y-1)/2+1,List.range(1,XL+1));
            allSquares = allSquares :+ s;
          };
          case 'x'  => {
            if(y%2 != 0){
              val neighbour = (((x-2)/4,(y-1)/2),((x+2)/4,(y-1)/2))
              neighbours = neighbours :+ neighbour;
            }else{
              val neighbour = ((x/4,((y-1)/2)),(x/4,y/2));
              neighbours = neighbours :+ neighbour;
            }
          };
          case ' ' =>;
          case _ => {
            val s = new Square(x/4+1,(y-1)/2+1, values = List(l(x).asDigit), solved = true);
            allSquares = allSquares :+ s;
          };
        }
      }
    }

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

    def printSolution():String = {
      var output:String = "size " + XL + "x" + XL +"\n";
      for(y<-List.range(1,XL+1)){
        for(x<- List.range(1,XL+1)){
          output+= getSquareXY(x,y).values(0) + " ";
        }
        output+="\n"
      }
      return output;
    }
    printSolution();
  }
}
