package fun

import image._
import java.awt.Graphics

object Fun6 {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/spiral/"+counter+" - " }
    
    //laziness
    type Dub = Double
    def cos(x:Dub) = math.cos(x)
    def sin(x:Dub) = math.sin(x)
    val pi = math.Pi
    def i(d:Dub) = d.toInt
    
    val paintColor = Color.white
    
    val left = (-1, 0)
    val up = (0, -1)
    val right = (1, 0)
    val down = (0, 1)
    def rotate(dir:(Int, Int)) = {
      dir match {
        case (-1, 0) => up
        case (0, -1) => right
        case (1, 0) => down
        case (0, 1) => left
      }
    }
    def sum(a:(Int, Int), b:(Int, Int)) = (a._1+b._1, a._2+b._2)
    def newDir(img:Image, pos:(Int, Int), dir:(Int, Int)):(Int, Int) = {
      val (nextX, nextY) = sum(pos, dir)
      if(!img.inBounds(nextX, nextY)) rotate(dir) //if you hit the edge, turn
      else if(img.get(nextX, nextY).get == paintColor) rotate(dir) //if you hit something you already painted, turn
      else dir //otherwise continue moving straight
    }
    def newPos(pos:(Int, Int), dir:(Int, Int)) = {
      sum(pos, dir)
    }
    
    val (w, h) = (50, 50)
    val img = new Image(w, h)
    var vec = (1, 0) //direction
    var pos = (0, 0)
    
    val speed = 4 //must be in range [1, w*h]
    for(t <- 1 to w*h/speed) {
      for(i <- 1 to speed) {
        //paint
        val (x, y) = pos
        img.img.setRGB(x, y, paintColor.rgb)
        
        //move turtle
        vec = newDir(img, pos, vec)
        pos = newPos(pos, vec)
      }
      
      img.toFile(dir+"frame")
    }
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
