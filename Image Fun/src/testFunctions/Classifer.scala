package testFunctions

import image.ComputerVision._
import scala.BigDecimal
import image.Color
import image.File
import image.Image

/*
 * Given some number of positive and negative examples,
 * can classify a given image as part of the positive examples
 * or the negative examples.
 * Uses local binary pattern to express each image as a vector.
 * 
 */

object Classifier {
  def learnResumable(imgDir:String, txtDir:String) {
    val imgFiles = File.getFilesIn(imgDir).filter { File.isImage }
    val numImgs = imgFiles.size
    var counter = 0
    for(imgFile <- imgFiles) {
      val relativePathString = imgFile.getPath.substring(imgDir.length)
      val txtPathString = (txtDir + relativePathString.dropRight(3)+"txt").replaceAll("\\\\", "/")
      val percentage = "%.3f".format(100.0*counter/numImgs)
      //if image not processed yet
      if(!File.exists(txtPathString)) {
        println(percentage+"% | processing "+relativePathString.tail)
        
        //process the image
        val img = File.readImage(imgFile)
        val binPercentages = img.localBinaryPatterns().getBinPercentages()
        
        //output the text to txtPathString
        val binsString = binPercentages.toList.mkString(" ")
        File.toFile(txtPathString, binsString)
      }
      else {
        println(percentage+"% | skipping   "+relativePathString.tail)
      }
      counter += 1
    }
  }
  
  //average all the vectors in the files in `filesDir` and write the average vector to `accumlatedFile`
  //ex usage: accumulateTxtFiles("res/test/positive", "res/test/average_positive.txt")
  def accumulateTxtFiles(filesDir:String, accumulatedFile:String) {
    var binsSum = Array.fill(256)(BigDecimal("0"))
    val files = File.getFilesIn(filesDir)
    println("avg "+files.size+" txt files to "+accumulatedFile)
    
    var points = scala.collection.mutable.ArrayBuffer[(Double, Double)]() //DEBUGGING
    
    for(file <- files) {
      val color = Color.random
      
      val bins = File.readLines(file).toList.head.split(" ").map{BigDecimal(_)}
      for(i <- 0 until bins.length) {
        binsSum(i) += bins(i)
        
        points += ((i.toDouble, bins(i).toDouble)) //DEBUGGING
      }
    }
    
    val fname = accumulatedFile.split("/").last //DEBUGGING
    Image.plotPoints(512, 512, points.toList).toFile("res/output/000 - bin amounts ("+fname+").jpg") //DEBUGGING
    
    val numFiles = BigDecimal(files.size+"")
    val binsString = binsSum.map{ x => (x / numFiles).toString() }.mkString(" ")
    File.toFile(accumulatedFile, binsString)
  }
  
  def isPositive(img:Image, avgPosPath:String=avgPositiveVector, avgNegPath:String=avgNegativeVector): Boolean = {
    def load(path:String) = File.readLines(path).toList.head.split(" ").map(_.toDouble)
    
    val avgNeg = load(avgNegPath)
    val avgPos = load(avgPosPath)
    
    def dist(a:Array[Double], b:Array[Double]):Double = {
      def sq(i:Double) = i*i
      (a, b).zipped.map{ _ - _ }.map{sq}.sum
    }
    
    val sample = img.localBinaryPatterns().getBinPercentages()
    
    val posDist = dist(sample, avgPos)
    val negDist = dist(sample, avgNeg)
    
//    println("posDist: "+posDist)
//    println("negDist: "+negDist)
    (posDist < negDist)
  }
  
  val allPositiveImages = "res/training/positive/large_set"
  val allNegativeImages = "res/training/negative"
  val allPositiveVectors = "res/training/learned/positive"
  val allNegativeVectors = "res/training/learned/negative"
  val avgPositiveVector = "res/training/learned/average_positive.txt"
  val avgNegativeVector = "res/training/learned/average_negative.txt"
  
  def main(args:Array[String]) {
    
    //learning
    learnResumable(allNegativeImages, allNegativeVectors)
    learnResumable(allPositiveImages, allPositiveVectors)
    accumulateTxtFiles(allPositiveVectors, avgPositiveVector)
    accumulateTxtFiles(allNegativeVectors, avgNegativeVector)
    
    //identification
    def testImg(path:String) {
      val isPosSample = isPositive(File.readImage(path))
      println( (if(isPosSample) "+" else "-")+" : "+path )
    }
    testImg("res/testing/positive/2.jpg")
    testImg("res/testing/negative/2.jpg")
  }
}
