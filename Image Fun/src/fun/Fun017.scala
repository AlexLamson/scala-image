package fun

import image._
import image.ComputerVision._
import java.awt.Graphics
import scala.util.Random

object Fun17 {
  var counter = 0
  def dir() = { counter += 1; "res/output/image stats/"+counter }
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
    
    
    
    val original = File.readImage("res/training/negative/3.jpg") //bedroom
    
    //MEDIAN COLOR (ONLINE ALGORITHM)
    val goalStepSize = 1
    var stepSize = 128.0
    var coolingRate = 1.1
    
    val directions = for{
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      if(!(x == 0 && y == 0 && z == 0))
    } yield (x, y, z)
    
    def distToAllOtherColors(c:Color, colors:List[Color]) = colors.map { _.manhattanDistanceTo(c) }.sum
    val allColors = original.iterator.toList
    var currMinColor = new Color(255/2, 255/2, 255/2)//avgHSVColor
    var currMinDist = distToAllOtherColors(currMinColor, allColors)
    
    //while we have insufficient precision
    while(stepSize > goalStepSize) {
      println("current best: "+currMinColor+" with distance "+currMinDist)
      var madeStep = false
      //for each neighboring direction
      for((x, y, z) <- directions) {
        //take a step
        val (h, s, v) = (currMinColor.h+x*stepSize, currMinColor.s+y*stepSize, currMinColor.v+z*stepSize)
        val testColor = Color.fromHsv(h, s, v)
        
        val dist = distToAllOtherColors(testColor, allColors)
        //keep the step if it's an improvement
        if(dist < currMinDist) {
          currMinColor = testColor
          currMinDist = dist
          madeStep = true
        }
      }
      //half the step size if no neighbors were better because we probably jumped too far
      if(!madeStep) {
        stepSize = stepSize/coolingRate
      }
    }
    val medianColor = currMinColor
    
    val medColorImage = Image.generate(500, 500, (_,_) => medianColor)
    medColorImage.toFile(dir+"median color (online algorithm)")
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
