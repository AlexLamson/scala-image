package fun

import image._
import java.awt.Graphics

object Fun3 {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/swinging/"+counter+" - " }
    
    def dist(a:Double, b:Double) = {
      def sq(x:Double) = x*x
      math.sqrt(sq(a-b))
    }
    
    def drawBall(x:Double, y:Double, r:Double, g:Graphics, t:Double) {
      def i(d:Double) = d.toInt
      
      //fill
      g.setColor(Color.fromHsv(t.toFloat, 0.8f, 0.8f).color)
      g.fillOval(i(x-r), i(y-r), i(r*2), i(r*2))
      
      //outline
      g.setColor(Color.black.color)
      g.drawOval(i(x-r), i(y-r), i(r*2), i(r*2))
      
      g.drawArc(i(x-r), i(y-r), i(r*2), i(r*2), 0, -180)
    }
    def getPos(w:Double, h:Double, t:Double) = {
      val ballX = (500.0/2*w)*math.cos(2*math.Pi*t)+(500.0/2)
      val ballY = (500.0/2*h)*math.sin(2*math.Pi*t)+(500.0/2)
      (ballX, ballY)
    }
    
    var ballSize = 40
    var arcWidth = 0.8
    var arcHeight = 0.4
    
    def drawBallFromT(t:Double, g:Graphics) {
      val (ballX, ballY) = getPos(arcWidth, arcHeight, t)
      drawBall(ballX, ballY, getBallSize(t-0*10*0.01), g, t-0*10*0.01)
    }
    
    def closeness(t:Double) = math.sin(t*2*math.Pi)/2.0+0.5
    def getBallSize(t:Double) = 2*ballSize*closeness(t)+10
    
//    for(t <- 0.0 until 0.02 by 0.01) {
    for(t <- 0.0 until 1.0 by 0.01) {
      val frame = Image.generate(500, 500, {g=>
//        drawBallFromT(t-2*10*0.01, g)
//        drawBallFromT(t-1*10*0.01, g)
//        drawBallFromT(t-0*10*0.01, g)
        
        val tVals = (9 to 0 by -1).map{t-_*10*0.01}.sortBy { t1 => math.sin(t1*2*math.Pi) }
        for(t1 <- tVals) {
          drawBallFromT(t1, g)
        }
//        for(i <- 9 to 0 by -1) {
//          drawBallFromT(t-i*10*0.01, g)
//        }
      })
      frame.toFile(dir+"frame")
    }
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
