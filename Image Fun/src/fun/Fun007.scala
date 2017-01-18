package fun

import image._
import image.ComputerVision._
import java.awt.Graphics

object Fun7 {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    var counter = 0
    def dir() = { counter += 1; "res/output/ball/"+counter }
    type Dub = Double
    def cos(x:Dub) = math.cos(x)
    def sin(x:Dub) = math.sin(x)
    val pi = math.Pi
    def i(d:Dub) = d.toInt
    
    
    //make a bouncing ball
    for(t <- 0 to 100) {
      val r = 10
      val img = Image.generate(500, 500, { g:Graphics =>
        g.setColor(Color.white.color)
        g.fillOval(i(1.0*t/100*500.0), i(250+250*sin(2*pi*t/100)), r*2, r*2)
      })
      img.toFile(dir+"frame")
    }
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
