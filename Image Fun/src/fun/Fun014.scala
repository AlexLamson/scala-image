package fun

import image._
import image.ComputerVision._
import java.awt.Graphics
import scala.util.Random

object Fun14 {
  var counter = 0
  def dir() = { counter += 1; "res/output/sorting/"+counter }
  type Dub = Double
  def cos(x:Dub) = math.cos(x)
  def sin(x:Dub) = math.sin(x)
  def abs(x:Dub) = math.abs(x)
  val pi = math.Pi
  def i(d:Dub) = d.toInt
  def rand(min:Double, max:Double) = math.random*(max-min)+min
  def sq(x:Dub) = x*x
  def dist(x1:Double, y1:Double, x2:Double, y2:Double) = math.sqrt(sq(x1-x2)+sq(y1-y2))
  
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    
//    //TODO DEBUG UNDERSTANDING NORMAL DISTRIBUTION
//    val numPoints = 10000
//    val normalNums = (1 to numPoints).map{ x => Random.nextGaussian() }.sorted
//    val points = (1 to numPoints).toList.zip(normalNums).map{ x => (x._1.toDouble, x._2) }
//    val (w, h) = (1920, 1080)
//    val imgPoints = Image.plotPoints(w, h, points)
//    imgPoints.toFile(dir+" normal points")
//    
//    
//    val arr = Array.fill(w, h)(0.0)
//    var i = 0
//    for(y <- 0 to h-1; x <- 0 to w-1) {
//      i += 1
//      if(100.0*i/(w*h) % 10 == 0) {
//        println((100*i/(w*h))+"% done")
//      }
//      
//      var sum = 0.0
//      for((x1, y1) <- points) {
//        sum += dist(x, y, x1, y1)
//      }
//      arr(x)(y) = sum
//    }
//    val max = arr.maxBy { _.max }.max
//    for(y <- 0 to h-1; x <- 0 to w-1) {
//      arr(x)(y) /= max
//    }
//    
//    val imgFlow = Image.generate(w, h, { (x, y) =>
//      Color.fromHsv(arr(x)(y), 0.9, 0.9)
//    })
//    imgFlow.toFile(dir+" flow")
//    
//    println("\n")
    
    
    
    
    //visualize shell sort using probabilistic comparators
    //IMPLEMENT HERE
    
    //TODO
    val (elementMin, elementMax) = (1, 100)
    val elementValueRange = abs(elementMax-elementMin)
    val (minDevPercent, maxDevPercent) = (0.1, 0.3)
    
    class Comparator {
      val devPercent = rand(minDevPercent, maxDevPercent)
      val standardDeviation = devPercent*(elementValueRange)+elementMin
      
      //returns -1 if a < b, 0 if a == b, 1 if a > b
      //depending on what it thinks is true
      //numbers farther from each other will be harder to get wrong
      def compare(a:Int, b:Int) = {
        
        val confusedA = Random.nextGaussian()*standardDeviation + a
        val diff = (confusedA-b)
        
        (diff/abs(diff)).toInt
      }
    }
    
//    val population = (1 to 1000).map{x => new Comparator()}
    val population = (1 to 1000).map{x => new Comparator()}.sortBy { x => 1.0-x.devPercent }
    val (x, y) = (30, 40)
    
    val guesses = population.map{ _.compare(x, y) }
    val weights = population.map{ x => 1.0 - (x.devPercent-minDevPercent)/(maxDevPercent-minDevPercent) }
    val classifiers = guesses.zip(weights)
    println("classifiers: "+classifiers)
    
    val average = 1.0*classifiers.map{ case (guess, weight) => guess }.sum/classifiers.length
    println("average: "+average)
    
    //TODO CHANGE INSTEAD OF SAYING 1-WEIGHT, USE THE Z-SCORE OF THE CLASSIFIER
    val adaboostedGuess = classifiers.map{ case (guess, weight) => guess*weight }.sum/classifiers.length
    println("adaboosted Guess: "+adaboostedGuess)
    
    val truth = {
      val diff = (x-y)
      (diff/abs(diff)).toInt
    }
    println("truth: "+truth)
    
//    for(user <- population) {
//      println(user.compare(a, b))
//    }
    
    
    
    
    //TODO
    //shell sort
    //https://rosettacode.org/wiki/Sorting_algorithms/Shell_sort#Scala
    object ShellSort {
      def greaterThan(a:Int, b:Int) = {
        a > b
        population.head.compare(a, b) == 1
      }
      
      def incSeq(len:Int) = new Iterator[Int]{
        private[this] var x:Int = len/2
        def hasNext = x > 0
        def next() = {x = if (x == 2) 1 else x*5/11; x}
      }
      
      def InsertionSort(a:Array[Int], inc:Int) = {
        for (i <- inc until a.length; temp = a(i)){
          var j = i
          while (j >= inc && greaterThan(a(j-inc), temp)){
            a(j) = a(j-inc)
            j = j-inc
            
            val (w, h) = (500, 500)
            val img = Image.histogram(w, h, a, Color.gray)
            img.toFile(dir+" step")
            
          }
          a(j) = temp
          
          val (w, h) = (500, 500)
          val img = Image.histogram(w, h, a, Color.lightGray)
          img.toFile(dir+" step")
        }
      }
      
      def shellSort(a:Array[Int]) = for(inc <- incSeq(a.length)) InsertionSort(a, inc)
    }
    var a = Array(2, 5, 3, 4, 3, 9, 3, 2, 5, 4, 1, 3, 10, 7, 2, -5, 8, 4)
    println(a.mkString(","))
    ShellSort.shellSort(a)
    println(a.mkString(","))
    
    val (w, h) = (500, 500)
    val img = Image.histogram(w, h, a, Color.green)
    img.toFile(dir+" step")
    
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
