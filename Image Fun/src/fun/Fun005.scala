package fun

import image._
import java.awt.Graphics

object Fun5 {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/tunnel/"+counter+" - " }
    
    //laziness
    type Dub = Double
    def cos(x:Dub) = math.cos(x)
    def sin(x:Dub) = math.sin(x)
    val pi = math.Pi
    def i(d:Dub) = d.toInt
    
    def drawCenteredOval(midX:Dub, midY:Dub, w:Dub, h:Dub, g:Graphics) {
      val x = midX-w/2
      val y = midY-h/2
      g.drawOval(i(x), i(y), i(w), i(h))
    }
    def fillCenteredOval(midX:Dub, midY:Dub, w:Dub, h:Dub, g:Graphics) {
      val x = midX-w/2
      val y = midY-h/2
      g.fillOval(i(x), i(y), i(w), i(h))
    }
    
    def makeFrame(t:Double):Image  = {
      val (w, h) = (500, 500)
      val img = new Image(w, h)
      val g = img.g
      val theta = t*2.0*pi
      
      val (eoW, eoH) = (200, 200) //earth orbit
      val (eW, eH) = (80, 80) //earth size
      val (moW, moH) = (60, 60) //moon orbit
      val (mW, mH) = (20, 20) //moon size
      
      val eSpeed = 1.0
      val mSpeed = 10.0
      
      val earthPos = (w/2+eoW*cos(eSpeed*theta), h/2+eoH*sin(eSpeed*theta))
      val moonLocalPos = (moW*cos(mSpeed*theta), moH*sin(mSpeed*theta))
      val moonPos = (earthPos._1+moonLocalPos._1, earthPos._2+moonLocalPos._2)
      
      //draw earth orbit
      g.setColor(Color.blue.color)
      drawCenteredOval(w/2, h/2, eoW*2, eoH*2, g)
      
      //draw earth
      g.setColor(Color.blue.color)
      fillCenteredOval(earthPos._1, earthPos._2, eW, eH, g)
      
      //draw moon orbit
      g.setColor(Color.red.color)
      drawCenteredOval(earthPos._1, earthPos._2, moW*2, moH*2, g)
      
      //draw moon
      g.setColor(Color.red.color)
      fillCenteredOval(moonPos._1, moonPos._2, mW, mH, g)
      
      img
    }
    
    for(t <- 0.0 until 1 by 0.01) {
      val img = makeFrame(t)
      img.toFile(dir+"frame")
    }
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
