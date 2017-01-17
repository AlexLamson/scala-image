package fun

import image._
import java.awt.Graphics

object Fun4 {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/spinner/"+counter+" - " }
    
    type Dub = Double
    def cos(x:Dub) = math.cos(x)
    def sin(x:Dub) = math.sin(x)
    val pi = math.Pi
    def i(d:Dub) = d.toInt
    def sq(x:Dub) = x*x
    def sqrt(x:Dub) = math.sqrt(x)
    
    
    def dist(x1:Int, y1:Int, x2:Int, y2:Int) = {
      sq(x2-x1)+sq(y2-y1)
    }
    def mod(x:Double) = x % 1.0
    
    val (w, h) = (500, 500)
    val maxDist = dist(0, 0, w/2, h/2)
    
    def f(x:Int, y:Int, t:Double=0):Color = {
      val theta = (math.atan2(y-(h/2), x-(w/2))+math.Pi)/(2.0*math.Pi)
      val d = dist(x, y, w/2, h/2)/maxDist
//      Color.fromHsv(d.toFloat, 0.8f, 0.8f)
//      Color.fromHsv((theta).toFloat, 0.8f, 0.8f)
//      Color.fromHsv(mod(theta+d+t).toFloat, 0.8f, 0.8f)
      
//      val newD = (sin(d*2*pi*2)+1.0)/2.0
//      val newD = (sin(sqrt(d)*2*pi*2*t)+1.0)/2.0
      val newD = (sin(d*2*pi*10*t)+1.0)/2.0
//      Color.fromHsv(mod(theta+newD+t).toFloat, 0.8f, 0.8f)
//      Color.fromHsv(mod(theta+d+t).toFloat, 0.8f, mod(newD).toFloat)
      
//      new Color(i(mod(theta+d+t)*255), i(0.8*255), i(mod(newD)*255))
      Color.fromYcbcr(i(mod(theta+d+t)*255), i((1.0-theta)*255), i(mod(newD)*255))
    }
    
//    for(t <- 0.0 until 0.02 by 0.01) {
    for(t <- 0.0 until 1 by 0.01) {
      val img = Image.generate(w, h, f(_, _, t))
      img.toFile(dir+"frame")
    }
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
