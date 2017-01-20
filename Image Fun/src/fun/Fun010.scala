package fun

import image._
import image.ComputerVision._
import java.awt.Graphics

object Fun10 {
  var counter = 0
  def dir() = { counter += 1; "res/output/vec field 2/"+counter }
  type Dub = Double
  def cos(x:Dub) = math.cos(x)
  def sin(x:Dub) = math.sin(x)
  val pi = math.Pi
  def i(d:Dub) = d.toInt
  def rand(min:Double, max:Double) = math.random*(max-min)+min
  def sq(x:Dub) = x*x
  def dist(x1:Double, y1:Double, x2:Double, y2:Double) = math.sqrt(sq(x1-x2)+sq(y1-y2))
  
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    
    
    val (w, h) = (500, 500)
    var (cX, cY) = (w/2, h/2)
    
    var t = 0.0
    
    def f(x:Double, y:Double):Vector = {
//      val theta = math.atan2(cY-y, cX-x)
//      val mag = (w/12)/longest*dist(x, y, cX, cY)
//      Vector.fromThetaMag(theta, mag)
      
      val c = w/12/2
      
//      new Vector(c*sin(x*2*pi/w)+c*sin(y*2*pi/h), c*cos(x*2*pi/w)+c*cos(y*2*pi/h))
//      new Vector(x*x, y*y)
//      new Vector(x, y)
//      new Vector(cos(x*2*pi/w), sin(y*2*pi/h))
//      new Vector(x*y, y*x)
//      new Vector(x, y)-new Vector(cX, cY)
//      new Vector(x, y)-new Vector(cX, cY) * (sin(6*2*pi*dist(x, y, cX, cY)/w)+1)
//      new Vector(sin(x*2*pi/w)+cos(y*2*pi/h), cos(x*2*pi/w)+sin(y*2*pi/h))
//      new Vector(x, y)-new Vector(cX, cY) * (sin(6*2*pi*dist(x, y, cX, cY)/w+(t*2*pi))+1)
      
      val xPerc = 1.0*x/w
      val yPerc = 1.0*y/h
      val angle = (math.atan2(cY-y, cX-x).toDegrees+180.0)/360.0
      val distance = dist(x, y, cX, cY)/( math.sqrt(sq(w/2)+sq(h/2)) )
      (new Vector(x, y) - new Vector(cX, cY)) + (t*1000)
    }
    
//    val longest = dist(0, 0, w/12, h/12)
    lazy val longest = {
      var max = 0.0
      for(x <- 0 to w-1; y <- 0 to h-1) {
        val funk = f(x, y).mag
        if(funk > max) max = funk
      }
      max
    }
    
    
    class Vector(val x:Double, val y:Double) {
      val theta = math.atan2(y, x)
      val mag = math.sqrt(sq(x)+sq(y))
      
      def normalize:Vector = Vector.fromThetaMag(theta, 1)
      
      def *(a:Double):Vector = Vector.fromThetaMag(theta, mag*a)
      
      def +(a:Double):Vector = new Vector(x+a, y+a)
      
      def -(v:Vector):Vector = new Vector(x-v.x, y-v.y)
      
      def toColor():Color = {
        def mod(a:Double, b:Double) = (a % b + b) % b
        val h = mod(theta.toDegrees, 360)/360.0
        
//        val h = (theta+pi)/2*pi
        val s = 0.8
        val v = mag/longest
        Color.fromHsv(h, s, v)
      }
      
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
    
    val vectorScaling = 0.1
    
    for(frameNum <- 1 to 10000) {
      t = 1.0*frameNum/100.0
      val img = Image.generate(w, h, f(_, _).toColor)
      println("rendering frame #"+frameNum)
      img.toFile(dir+" frame")
    }
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
