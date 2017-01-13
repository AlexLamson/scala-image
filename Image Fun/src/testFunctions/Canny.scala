package testFunctions

import image.ComputerVision._
import image.File

object Canny {
  def main(args:Array[String]) {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/"+counter+" - " }
    
    
    println("reading file")
//    val original = File.readImage("res/testing/negative/2.jpg") //elephant
    val original = File.readImage("res/training/negative/3.jpg") //bedroom
//    val original = File.readImage("res/testing/negative/6.jpg") //2 vert stripes
//    val original = File.readImage("res/testing/negative/7.jpg") //2 horz stripes
    original.toFile(dir+"original")
    
    println("blurring image")
    val blurredImg = original.blur()
    blurredImg.toFile(dir+"blurred")
    
    println("running sobel filter")
    val sobelImg = blurredImg.sobel
    sobelImg.toImage(colored=true).toFile(dir+"sobel")
    
    println("running canny filter")
    val cannyImg = sobelImg.canny()
    cannyImg.toImage().toFile(dir+"canny")
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
