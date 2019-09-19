import java.io._;
import Solver.SolveSquare;

class IO {
  val inputdir = "src/Input/";
  import java.io.File;

  val dir = new File(inputdir);
  for(f<-dir.listFiles()){
    solveSlitherLinks(f)
  }

  def solveSlitherLinks(f:File):Unit = {
    println(f.getName());
    val lines = scala.io.Source.fromFile(f).mkString.split("\n")

    val XL = lines(1).toCharArray()(5).asDigit;
    val matrix = Array.ofDim[Int](XL,XL);

    var allSquares = List[Square]();
    var neighbours : List[((Int,Int),(Int,Int))] = List()
    val dn = List[(Int,Square)]();
    for(y<- 2 to lines.length-1){
      val l = lines(y).toCharArray;
      for(x<- 0 until l.length by 2){
        println(l(x))
        l(x) match{
          case '_' => {
            matrix((y-2)/2)(x/4) = 0
            val s = new Square(x/4+1,(y-2)/2 +1,neighbour =  dn);
            allSquares = allSquares :+ s;
          };
          case 'x'  => {
            if(y%2 == 0){
              println("sbs neighbour")
              val neighbour = (((x-2)/4,(y-2)/2),((x+2)/4,(y-2)/2))
              neighbours = neighbours :+ neighbour;
              println(neighbour);
            }else{
              println("top neighbour")
              val neighbour = ((x/4,((y-1)/2)),(x/4,(y-3)/2));
              neighbours = neighbours :+ neighbour;
              println(neighbour)
            }
          };
          case ' ' =>;
          case _ => {
            matrix((y-2)/2)(x/4) = l(x).asDigit
            val s = new Square(x/4+1,(y-2)/2+1, values = List(l(x).asDigit), solved = true, neighbour = dn);
            allSquares = allSquares :+ s;
          };
        }
      }
    }

    def getSquareXY(x:Int, y:Int):Square = {
      if(x > 4 || y > 4 || x ==0 || y ==0) return null
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

    allSquares = Solver.SolveSquare(allSquares,XL);

    def printSolution() = {
      var output:String = ""
      for(y<-List(1,2,3,4)){
        for(x<- List(1,2,3,4)){
          var s = getSquareXY(x,y);
          if(s.values.length == 1) output += s.values(0) + "|"
          else {output +=   "x|"}
        }
        output+="\n"
      }
      print(output)
    }
    printSolution();
  }
}
