package fun

import image._
import image.ComputerVision._
import java.awt.Graphics

object Fun9 {
  var counter = 0
  def dir() = { counter += 1; "res/output/vec field/"+counter }
  type Dub = Double
  def cos(x:Dub) = math.cos(x)
  def sin(x:Dub) = math.sin(x)
  val pi = math.Pi
  def i(d:Dub) = d.toInt
  def rand(min:Double, max:Double) = math.random*(max-min)+min
  def sq(x:Dub) = x*x
  
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    
    
    class Vector(val x:Double, val y:Double) {
      val theta = math.atan2(y, x)
      val mag = math.sqrt(sq(x)+sq(y))
      
      def normalize:Vector = Vector.fromThetaMag(theta, 1)
      
      def *(a:Double):Vector = Vector.fromThetaMag(theta, mag*a)
      
      def render(g:Graphics, xPos:Int, yPos:Int) {
        //draw line
        g.setColor(Color.white.color)
        g.drawLine(i(xPos-x/2), i(yPos-y/2), i(xPos+x/2), i(yPos+y/2))
        
        //draw arrow head
        val arrowSize = 10
        val arrowAngle = pi/6.0
        
        val px = i(xPos+x/2)
        val py = i(yPos+y/2)
        
        val p1x = i(px+(arrowSize*cos(theta+pi-arrowAngle)))
        val p1y = i(py+(arrowSize*sin(theta+pi-arrowAngle)))
        
        val p2x = px
        val p2y = py
        
        val p3x = i(px+(arrowSize*cos(theta+pi+arrowAngle)))
        val p3y = i(py+(arrowSize*sin(theta+pi+arrowAngle)))
        
        g.setColor(Color.white.color)
        g.fillPolygon(Array(p1x, p2x, p3x), Array(p1y, p2y, p3y), 3)
      }
    }
    object Vector {
      def fromThetaMag(theta:Double, mag:Double): Vector = {
        new Vector(mag*cos(theta), mag*sin(theta))
      }
    }
    
    def dist(x1:Double, y1:Double, x2:Double, y2:Double) = {
      math.sqrt(sq(x1-x2)+sq(y1-y2))
    }
    
    val (w, h) = (500, 500)
    var (cX, cY) = (w/2, h/2)
    
    val longest = dist(w/12, h/12, cX, cY)
    def f(x:Double, y:Double):Vector = {
//      val theta = math.atan2(cY-y, cX-x)
//      val mag = (w/12)/longest*dist(x, y, cX, cY)
//      Vector.fromThetaMag(theta, mag)
      
      val c = w/12/2
      
//      new Vector(c*sin(x*2*pi/w)+c*sin(y*2*pi/h), c*cos(x*2*pi/w)+c*cos(y*2*pi/h))
      new Vector(x*x, y*y)
    }
    
    val vectorScaling = 0.1
    
    var mikeRadius = 20
    var (mikeX, mikeY) = (455.0, 356.0)//(1.0*w/2/2, 20.0/2)
    var mikeVec = new Vector(10, -30) * vectorScaling
    
    for(frameNum <- 1 to 1) {
      val img = Image.generate(w, h, { g:Graphics =>
//        g.setColor(Color.red.color)
//        g.drawLine(0, 0, 100, 100)
        
//        val v = new Vector(100, 200)
//        v.render(g, w/2, h/2)
        
        
        for(yPos <- h/12 to h-h/12 by h/12; xPos <- w/12 to w-w/12 by w/12) {
          
          val v = f(xPos, yPos)
          v.render(g, xPos, yPos)
        }
        
        val v = f(mikeX, mikeY) * vectorScaling
        g.setColor(Color.cyan.color)
        g.fillOval(i(mikeX), i(mikeY), mikeRadius, mikeRadius)
        mikeVec = new Vector(mikeVec.x+v.x, mikeVec.y+v.y)
        mikeX += mikeVec.x
        mikeY += mikeVec.y
        
      })
      img.toFile(dir+" frame")
    }
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
