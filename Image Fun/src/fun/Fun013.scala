package fun

import image._
import image.ComputerVision._
import java.awt.Graphics

object Fun13 {
  var counter = 0
  def dir() = { counter += 1; "res/output/fractal tree/"+counter }
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
    
    var lengthFactor = 0.5
    var angleDelta = 45.0
    val recursionDegree = 3
    
    def recurse( g:Graphics, counter:Int, counterLimit:Int, length:Dub, angle:Dub, x:Dub, y:Dub ) {
      if(counter < counterLimit) {
        val endX = x+length*cos(angle.toRadians)
        val endY = y+length*sin(angle.toRadians)
        
//        g.setColor(Color.white.color)
        g.setColor(Color.fromHsv(angle/360, 0.9, 1.0-(1.0*counter/counterLimit)).color)
//        g.drawLine(i(x), i(y), i(endX), i(endY))
        
        import java.awt.Graphics2D
        import java.awt.BasicStroke
        import java.awt.geom.Line2D
        
        val g2 = g match {
            case g2: Graphics2D => g2
            case _ => throw new ClassCastException
          }
        g2.setStroke(new BasicStroke( i(20*(1.0-(1.0*counter/counterLimit))) ));
        g2.draw(new Line2D.Float(i(x), i(y), i(endX), i(endY)));
        
//        recurse(g, counter+1, counterLimit, length*lengthFactor, angle-angleDelta, endX, endY)
//        recurse(g, counter+1, counterLimit, length*lengthFactor, angle+angleDelta, endX, endY)
        
        for(newAngleDelta<- (-angleDelta) to (angleDelta) by (angleDelta*2/(recursionDegree-1))) {
          recurse(g, counter+1, counterLimit, length*lengthFactor, angle+newAngleDelta, endX, endY)
        }
      }
    }
    
    
    val frames = 100
    for(frame <- 1 to frames) {
      val percent = 1.0*frame/frames
      val (minAngle, maxAngle) = (15, 120)
      angleDelta = ((maxAngle-minAngle)*percent)+minAngle
      
      //render fractal
      val img = Image.generate(w, h, { g:Graphics =>
        recurse(g, 0, 7, 200, 270, w/2, h)
      })
      img.toFile(dir+" tree")
      println("rendered frame "+frame+" / "+frames)
    }
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
