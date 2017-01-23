package fun

import image._
import image.ComputerVision._
import java.awt.Graphics
import scala.util.Random

object Fun15 {
  var counter = 0
  def dir() = { counter += 1; "res/output/digits/"+counter }
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
    
    
    
    val lines = File.readLines("res/output/digits/digits.csv").toList
    for(line <- lines) {
      
      val sides = line.split(", ")
      val binaryValues = sides(0).split(" ").map{_.toInt == 1}
      val digitValue = sides(1).toInt
      
      def getColor(x:Int, y:Int) = if(binaryValues(x+y*16)) Color.white else Color.black
      
      
      val (w, h) = (16, 16)
      val img = Image.generate(w, h, {(x, y) => getColor(x, y)})
      img.toFile(dir+" digit ("+digitValue+")")
    }
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
