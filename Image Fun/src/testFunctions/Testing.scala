package testFunctions

import image._
import image.ComputerVision._
import image.File

object Testing {
  def main(args:Array[String]) {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/"+counter+" - " }
    
    
    println("reading file")
//    val original = File.readImage("res/testing/negative/2.jpg") //elephant
//    val original = File.readImage("res/training/negative/3.jpg") //bedroom
//    val original = File.readImage("res/testing/negative/3.jpg") //camping
    val original = File.readImage("res/testing/negative/4.jpg") //forest
//    val original = File.readImage("res/testing/negative/6.jpg") //2 vert stripes
//    val original = File.readImage("res/testing/negative/7.jpg") //2 horz stripes
    original.toFile(dir+"original")
    
    
//    //AVERAGE COLOR
//    val numberOfPixels = original.width*original.height
//    var weightedHueSum = 0.0
//    var satSum = 0.0
//    var valSum = 0.0
//    for(color <- original.iterator) {
//      weightedHueSum += color.s * color.h
//      satSum += color.s
//      valSum += color.v
//    }
//    val avgHue = weightedHueSum / numberOfPixels
//    val avgSat = satSum / numberOfPixels
//    val avgVal = valSum / numberOfPixels
//    val avgHSVColor = Color.fromHsv(avgHue, avgSat, avgVal)
//    
//    val avgColorImage = Image.generate(500, 500, (_,_) => avgHSVColor)
//    avgColorImage.toFile(dir+"average color")
    
    
//    //MEDIAN COLOR
////    def distToAllOtherColors(c:Color, colors:List[Color]) = colors.map { _.distanceTo(c) }.sum
//    def distToAllOtherColors(c:Color, colors:List[Color]) = colors.map { _.manhattanDistanceTo(c) }.sum
//    
//    val allColors = original.iterator.toList
//    val medianColor = allColors.minBy { distToAllOtherColors(_, allColors) }
//    
//    val medColorImage = Image.generate(500, 500, (_,_) => medianColor)
//    medColorImage.toFile(dir+"median color")
    
    
    //MEDIAN COLOR (ONLINE ALGORITHM)
    val goalStepSize = 1
    var stepSize = 128.0
    var coolingRate = 1.1
    
    val directions = for{
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      if(!(x == 0 && y == 0 && z == 0))
    } yield (x, y, z)
    
    def distToAllOtherColors(c:Color, colors:List[Color]) = colors.map { _.manhattanDistanceTo(c) }.sum
    val allColors = original.iterator.toList
    var currMinColor = new Color(255/2, 255/2, 255/2)//avgHSVColor
    var currMinDist = distToAllOtherColors(currMinColor, allColors)
    
    //while we have insufficient precision
    while(stepSize > goalStepSize) {
      println("current best: "+currMinColor+" with distance "+currMinDist)
      var madeStep = false
      //for each neighboring direction
      for((x, y, z) <- directions) {
        //take a step
        val (h, s, v) = (currMinColor.h+x*stepSize, currMinColor.s+y*stepSize, currMinColor.v+z*stepSize)
        val testColor = Color.fromHsv(h, s, v)
        
        val dist = distToAllOtherColors(testColor, allColors)
        //keep the step if it's an improvement
        if(dist < currMinDist) {
          currMinColor = testColor
          currMinDist = dist
          madeStep = true
        }
      }
      //half the step size if no neighbors were better because we probably jumped too far
      if(!madeStep) {
        stepSize = stepSize/coolingRate
      }
    }
    val medianColor = currMinColor
    
    val medColorImage = Image.generate(500, 500, (_,_) => medianColor)
    medColorImage.toFile(dir+"median color (online algorithm)")
    
    
    
//    val scaled = original.scale(original.width*2, original.height*2)
//    scaled.toFile(dir+"scaled")
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
