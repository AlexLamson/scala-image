package fun

import image._

object Fun2 {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/graphy/"+counter+" - " }
    
    type Vector = (Double, Double)
    implicit class MagicVector(a: (Double, Double)) {
      def distance(b:Vector) = {
        def sq(x:Double) = x*x
        math.sqrt(sq(a._1-b._1)+sq(a._2-b._2))
      }
      def +(b:Vector):Vector = {
        (a._1+b._1, a._2+b._2)
      }
      def -(b:Vector):Vector = {
        (a._1-b._1, a._2-b._2)
      }
      def *(x:Double):Vector = {
        (a._1*x, a._2*x)
      }
    }
    object Vector {
      def random() = {
        val angle = (360*math.random).toRadians
        (math.cos(angle), math.sin(angle))
      }
      def random(low1:Double, high1:Double, low2:Double, high2:Double) = {
        def ran1 = (math.random*(high1-low1))+low1
        def ran2 = (math.random*(high2-low2))+low2
        (ran1, ran2)
      }
    }
    
    class Dot(var pos:Vector, var vel:Vector) {
      val mass = rand(2, 10)
      
      def x = pos._1
      def y = pos._2
      
      def tick {
        pos += vel
      }
      def render(g:java.awt.Graphics) {
        val r = 2
        g.setColor(Color.white.color)
        g.fillOval((x-r/2).toInt, (y-r/2).toInt, r, r)
      }
    }
    
    def rand(low:Double, high:Double) = math.random*(high-low)+low
    
    class World {
      val (width, height) = (500, 500)
      def randPos = (width*math.random, height*math.random)
      def randDot = new Dot(randPos, Vector.random*rand(0, 10.0))
      
      val dots = (1 to 300).map{_ => randDot}.toList
      
      def tick {
        dots.map{ x =>
          x.tick
          if(x.x > width || x.x < 0) {
            x.pos = (x.x % width, x.pos._2)
          }
          if(x.y > height || x.y < 0) {
            x.pos = (x.pos._1, x.y % height)
          }
        }
      }
      
      def toImage() = {
        val img = new Image(width, height)
        val g = img.g
        
        //draw the dots
        dots.map{_.render(g)}
        
        def drawLines(a:Dot, otherDots:List[Dot]) {
          for(b <- otherDots) {
            if(a != b) {
              
              val dist = a.pos.distance(b.pos)
              if(dist <= 50 && dist > 0) {
                //draw the lines between the dots
                val (x1, y1) = a.pos
                val (x2, y2) = b.pos
                g.setColor(Color.white.color)
                g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
                
                
                //impart gravity onto the 2 dots
                val G = 1.0
                val F_g = G*a.mass*b.mass/(dist*dist)
                
                val (xA, yA) = a.pos - b.pos
                val angleA = math.atan2(yA, xA)
                a.vel = (F_g*math.cos(angleA), F_g*math.sin(angleA))
                
//                val (xB, yB) = b.pos - a.pos
//                val angleB = math.atan2(yB, xB)
//                b.vel = (F_g*math.cos(angleB), F_g*math.sin(angleB))
              }
              
            }
          }
        }
        
//        drawLinesRecur(dots)
//        def drawLinesRecur(dots:List[Dot]) {
//          dots match {
//            case Nil => Unit //do nothing
//            case x :: xs => {
//              drawLines(x, xs)
//              drawLinesRecur(xs)
//            }
//          }
//        }
        
        for(a1 <- 0 to dots.length-1) {
          for(b1 <- a1 to dots.length-1) {
            val (a, b) = (dots(a1), dots(b1))
            
            if(a != b) {
              
              val dist = a.pos.distance(b.pos)
              if(dist <= 50 && dist > 0) {
                //draw the lines between the dots
                val (x1, y1) = a.pos
                val (x2, y2) = b.pos
                g.setColor(Color.white.color)
                g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
                
                
                //impart gravity onto the 2 dots
                val G = 100.0
                val F_g = -G*a.mass*b.mass/(dist*dist)
                
                val (xA, yA) = a.pos - b.pos
                val angleA = math.atan2(yA, xA)
                a.vel = (F_g*math.cos(angleA), F_g*math.sin(angleA))
              }
              
            }
          }
        }
        
        img
      }//end of toImage
      
    }//end of World
    
    val world = new World()
    for(i <- 1 to 100) {
      world.toImage.toFile(dir+"graphy")
      world.tick
    }
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
