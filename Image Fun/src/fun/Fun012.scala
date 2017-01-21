package fun

import image._
import image.ComputerVision._
import java.awt.Graphics

object Fun12 {
  var counter = 0
  def dir() = { counter += 1; "res/output/curve/"+counter }
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
    
    
    //spline junk (incomplete)
    
    def drawPoint(x:Dub, y:Dub, g:Graphics) {
      val r = 10.0
      g.setColor(Color.red.color)
      g.fillOval(i(x-r), i(y-r), i(r*2), i(r*2))
    }
    
    def drawLine(x1:Dub, y1:Dub, x2:Dub, y2:Dub, g:Graphics) {
      g.setColor(Color.blue.color)
      g.drawLine(i(x1), i(y1), i(x2), i(y2))
    }
    
    
    val (w, h) = (500, 500)
    val points = List( (40, 400), (50, 60), (370, 100) )
    
    val pointPairs = (points.last :: points).sliding(2).toList
    
    val img = Image.generate(w, h, { g:Graphics =>
      points.foreach{ p => drawPoint(p._1, p._2, g) }
      
      pointPairs.foreach{pair=>
        val p1 = pair.head
        val p2 = pair.last
        drawLine(p1._1, p1._2, p2._1, p2._2, g)
      }
    })
    img.toFile(dir+" curve")
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
