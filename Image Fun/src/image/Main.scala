package image

import ComputerVision._

/*
 * todo:
 * -add kernel generation & application methods to Image
 * -implement canny filter
 *   -constructor should take SobelImg as arg by default
 *   -secondary constructor should take Image
 * -clean up images
 * -implement auto threshold
 * -implement sharpen
 * 
 */

object Main {
  def main(args: Array[String]) {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/"+counter+" - " }
    
    
    println("reading file")
//    val original = File.readImage("res/training/negative/3.jpg")
    val original = File.readImage("res/training/negative/2.jpg")
//    val original = File.readImage("res/training/positive/1.jpg")
//    val original = File.readImage("res/training/positive/2.jpg").constrainTo(1000, 1000)
//    val original = File.readImage("res/testing/1.jpg").constrainTo(1000, 1000)
//    val original = File.readImage("res/training/negative/3.jpg").constrainTo(1000, 1000)
    original.toFile(dir+"original")
    
    println("running gaussian blur")
    val blurImg = original.blur(1)
    blurImg.toFile(dir+"guassian blur")
    
    println("running local binary patterns")
    val lbpImg = original.localBinaryPatterns().toImage()
    lbpImg.toFile(dir+"local binary patterns")
    
    println("making pattern count histogram")
    val patternHistogram = original.localBinaryPatterns().toImageHistogram()
    patternHistogram.toFile(dir+"binary pattern count histogram")
    
    println("running sobel filter")
    val sobelImg = new SobelImage(original).toImageColored()
    sobelImg.toFile(dir+"sobel")
    
    println("running k-means")
    val scaled = original.constrainTo(500, 500)
//    File.toFile("res/output/convergence.txt", "") //clear out the file
    val kMeans = scaled.kMeans(20, { (clusters, movement, iterationNum) =>
        println("k-means convergence: "+movement)
//        File.appendToFile("res/output/convergence.txt", movement+"\n")
        
//        val clustersImg = Cluster.clustersToImage(clusters)
        //colored clusters
//        clustersImg.toFile("res/output/kmeans/0 - kmeans iteration #"+iterationNum+".jpg")
        //labeled colors
//        val stringsAndPoints = clusters.map { x => (x.color.toName, x.pos) }.toList
//        clustersImg.addLabels(stringsAndPoints).toFile("res/output/kmeans/0 - kmeans iteration #"+iterationNum+".jpg")
        
    })
    val stringsAndPoints = kMeans.clusters.map { x => (x.color.toName, x.pos) }.toList
    val kMeansImg = kMeans.toImage().addLabels(stringsAndPoints)
    kMeansImg.toFile(dir+"k-means")
    
//    println("generating convergence graph")
//    val lines = File.readLines("res/output/convergence.txt").map{_.toDouble}.toList
//    val numbers = List.range(0, lines.length).map { _.toDouble }
//    val points = numbers.zip{lines}
//    val convergenceImg = Image.plotPoints(points, smallPoints=false, drawLine=true)
//    convergenceImg.toFile(dir+"convergence")
    
    println("running histogram equalization")
    val equalizedImg = original.equalize()
    equalizedImg.toFile(dir+"equalized")
    
    println("drawing rgb histogram")
    original.histogramRGB().toFile(dir+"rgb histogram")
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
