package fun

import image._
import image.ComputerVision._
import java.awt.Graphics
import scala.util.Random

object Fun16 {
  var counter = 0
  def dir() = { counter += 1; "res/output/random uniform circle/"+counter }
  type Dub = Double
  def cos(x:Dub) = math.cos(x)
  def sin(x:Dub) = math.sin(x)
  def abs(x:Dub) = math.abs(x)
  val pi = math.Pi
  def i(d:Dub) = d.toInt
  def rand(min:Double, max:Double) = math.random*(max-min)+min
  def sq(x:Dub) = x*x
  def dist(x1:Double, y1:Double, x2:Double, y2:Double) = math.sqrt(sq(x1-x2)+sq(y1-y2))
  
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    
    
    //non-uniform way
//    def makePoint(x:Dub, y:Dub, r:Dub):(Dub, Dub) = {
//      val t = 2.0*pi*math.random
//      val radius = r*math.random
//      (x+radius*cos(t), y+radius*sin(t))
//    }
    
    //uniform way
    def makePoint(x:Dub, y:Dub, r:Dub):(Dub, Dub) = {
      val t = 2.0*pi*math.random
      val u = math.random+math.random
      val radius = r*(if(u > 1) 2.0-u else u)
      (x+radius*cos(t), y+radius*sin(t))
    }
    
    val (w, h) = (500, 500)
    
    for(i <- 1 to 1) {
      val points = (1 to 100000).map{ _ => makePoint(0, 0, 1) }.toList
      val img = Image.plotPoints(w, h, points, smallPoints=true)
      img.toFile(dir+"points")
    }
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
