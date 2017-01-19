package fun

import image._
import image.ComputerVision._
import java.awt.Graphics

object Fun8 {
  var counter = 0
  def dir() = { counter += 1; "res/output/svm/"+counter }
  type Dub = Double
  def cos(x:Dub) = math.cos(x)
  def sin(x:Dub) = math.sin(x)
  val pi = math.Pi
  def i(d:Dub) = d.toInt
  def rand(min:Double, max:Double) = math.random*(max-min)+min
  def sq(x:Dub) = x*x
  
  object Optimize {
    def regressionCost(a:Dub, b:Dub, points:List[(Int, Int)]) = {
      val sumOfSquares = { for((x, y) <- points) yield sq((a*x+b)-y) }.sum
      sumOfSquares
    }
    
    def binaryClassificationCost(a:Dub, b:Dub, posPoints:List[(Int, Int)], negPoints:List[(Int, Int)]) = {
      
      val numTruePositives = posPoints.count{ case (x, y) => a*x+b < y }
      val numFalseNegatives = posPoints.length - numTruePositives
      val numTrueNegatives = negPoints.count{ case (x, y) => a*x+b > y }
      val numFalsePositives = negPoints.length - numTrueNegatives
      
      -(numFalseNegatives + numFalsePositives)
    }
  }
  
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    def makePoint(x:Dub, y:Dub, r:Dub):(Int, Int) = {
      val t = 2.0*pi*math.random
      val u = math.random+  math.random
      val radius = r*(if(u > 1) 2.0-u else u)
      (i(x+radius*cos(t)), i(y+radius*sin(t)))
//      val theta = rand(0, 359).toRadians
//      ((x+cos(theta)*rand(0, r)).toInt, (y+sin(theta)*rand(0, r)).toInt)
    }
    
    println("generating points")
    val pointsA = {for(i <- 1 to 100) yield makePoint(100, 135, 250*2)}.toList
    val pointsB = {for(i <- 1 to 100) yield makePoint(390, 310, 160*2)}.toList
    
//    println("optimizing")
//    var (bestA, bestB, bestCost) = (0.0, 0.0, Double.MaxValue)
//    for(a <- -10.0 to 10.0 by 0.01; b <- 0.0 to 500.0 by 0.5) {
////      val cost = Optimize.regressionCost(a, b, pointsA.union(pointsB))
//      val cost = Optimize.binaryClassificationCost(a, b, pointsA, pointsB)
//      if(cost < bestCost) {
//        bestA = a
//        bestB = b
//        bestCost = cost
//      }
//    }
//    println("bestA: "+bestA+" bestB: "+bestB)
    
//    val img = Image.generate(500, 500, { g:Graphics =>
//      g.setColor(Color.red.color)
//      for((x, y) <- pointsA) g.drawOval(x, y, 10, 10)
//      
//      g.setColor(Color.cyan.color)
//      for((x, y) <- pointsB) g.drawOval(x, y, 10, 10)
//      
//      g.setColor(Color.magenta.color)
//      g.drawLine(0, (0*bestA+bestB).toInt, 500, (500*bestA+bestB).toInt)
//    })
//    img.toFile(dir+"points")
    
    println("optimizing")
    var (bestA, bestB, bestCost) = (0.0, 0.0, Double.MaxValue)
    for(a <- -10.0 to 10.0 by 0.01; b <- 0.0 to 1000.0 by 0.5) {
//      val cost = Optimize.regressionCost(a, b, pointsA.union(pointsB))
      val cost = Optimize.binaryClassificationCost(a, b, pointsA, pointsB)
      if(cost < bestCost) {
        bestA = a
        bestB = b
        bestCost = cost
        
        render()
      }
    }
    println("bestA: "+bestA+" bestB: "+bestB)
//    render()
    
    def render() {
      val img = Image.generate(500, 500, { g:Graphics =>
          g.setColor(Color.red.color)
          for((x, y) <- pointsA) g.drawOval(x, y, 10, 10)
          
          g.setColor(Color.cyan.color)
          for((x, y) <- pointsB) g.drawOval(x, y, 10, 10)
          
          g.setColor(Color.magenta.color)
          g.drawLine(0, (0*bestA+bestB).toInt, 500, (500*bestA+bestB).toInt)
        })
        img.toFile(dir+"points")
    }
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
