package fun

import image._
import scala.util.Random

object Fun18E {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/ellie_squares/"+counter+" - " }
    
    
    println("reading file")
    val original = File.readImage("res/output/ellie_squares/0_ellie.png")
//    original.toFile(dir+"original")
    
    val copyImg = new Image(original.width, original.height)
    val g = copyImg.g
    val minSize = math.min(copyImg.width, copyImg.height)
    
    
    
    def randomPos = ((math.random*copyImg.width).toInt, (math.random*copyImg.height).toInt)
    
    println("drawing frames")
    
    val a = 200.0
    val b = 20.0
    def y(x:Double) = a*math.pow(math.E, -x/b)
    
    val iterations = 100
    for(i <- 1 to iterations) {
      
      val percentComplete = 1.0*i/iterations
      
      val originalCircleSize = (y(i)).toInt
      val circleSize = mutateCircleSize(originalCircleSize, percentComplete)
      
      for(j <- 1 to (copyImg.width/circleSize)*(copyImg.height/circleSize)) {
//      for(j <- 1 to 100) {
        val (x, y) = randomPos
        val color = original.get(x, y).get
        val mutColor = mutateColor(color, percentComplete)
        g.setColor(mutColor.color)
        g.fillOval(x-circleSize/2, y-circleSize/2, circleSize, circleSize)
//        g.fillRect(x-circleSize/2, y-circleSize/2, circleSize, circleSize)
      }
      
      copyImg.toFile(dir+"squares")
    }
    
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
  
  def mutateCircleSize(size:Double, percentComplete:Double) = {
    val variance = (1.0-percentComplete)*10
    
    def randDelta = Random.nextGaussian()*variance
    
    math.max((size + randDelta).toInt, 1)
  }
  
  def mutateColor(c:Color, percentComplete:Double) = {
    var (r, g, b) = (c.r, c.g, c.b)
    
    val variance = (1.0-percentComplete)*20
    
    def randDelta = (Random.nextGaussian()*variance).toInt
    
    r += randDelta
    g += randDelta
    b += randDelta
    
    def constrain(a:Int, b:Int, c:Int) = {
      def bound(i:Int) = math.min( math.max(0,i) ,255)
      (bound(a), bound(b), bound(c))
    }
    
    new Color(constrain(r, g, b))
  }
}
