package testFunctions

import image._
import image.ComputerVision._

object Convolve {
  def main(args:Array[String]) {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/"+counter+" - " }
    
    
    println("reading file")
    val original = File.readImage("res/testing/negative/2.jpg") //elephant
//    val original = File.readImage("res/training/negative/3.jpg") //bedroom
//    val original = File.readImage("res/testing/negative/6.jpg") //2 vert stripes
//    val original = File.readImage("res/testing/negative/7.jpg") //2 horz stripes
//    val original = File.readImage("res/training/negative/lenna.png") //lenna
//    val original = File.readImage("res/training/negative/test.png") //debugging
    original.toFile(dir+"original")
    
    
//    val kernel = Kernel.make2D("0 0 0;0 1 0;0 0 0") //identity
    
//    var kernel = Array.fill(5, 5)(1.0/(5*5)) //blur
//    var kernel = Array.fill(5, 5, 5)(1.0/(5*5*5)) //colored blur
    
//    val kernel = Kernel.make2D("0 1 0;1 -4 1;0 1 0") //edge detection
//    val kernel = Kernel.make2D("-1 -2 -1;0 0 0;1 2 1") //vertical edges
    val kernel = Kernel.make2D("-1 0 1;-2 0 2;-1 0 1") //horizontal edges
    
//    val kernel = Kernel.make2D("0 -1 0;-1 5 -1;0 -1 0") //sharpen
//    val kernel = Kernel.make2D("0 0 -1 0 0;0 -1 -2 -1 0;1 -2 16 -2 -1;0 -1 -2 -1 0;0 0 -1 0 0") //sharpen
    
    //convolve single channel
//    val arr = original.convolve(kernel, _.v)
//    Image.from2dIntArray(arr).toFile(dir+"convolved")
    
    //convolve rgb channels
//    val (rArr, gArr, bArr) = original.convolve(kernel, kernel, kernel)
//    Image.from2dIntArrays(rArr, gArr, bArr).toFile(dir+"convolved")
    
    //convolve arbitrarily chosen channels
    val kernelA = Kernel.make2D("-1 -2 -1;0 0 0;1 2 1") //vertical edges
    val kernelB = Kernel.make2D("-1 0 1;-2 0 2;-1 0 1") //horizontal edges
    val kernelC = Kernel.make2D("0 -1 0;-1 5 -1;0 -1 0") //sharpen
//    val (rArr, gArr, bArr) = original.convolveCray(kernelA, kernelB, kernelC)
//    Image.from2dIntArrays(rArr, gArr, bArr).toFile(dir+"convolved")
    
    val arrA = original.convolve(kernelA, _.v)
    val arrB = original.convolve(kernelB, _.v)
    (Image.from2dIntArray(arrA)+Image.from2dIntArray(arrB)).toFile(dir+"convolved")
    
    original.sobel().toImage(colored=false).toFile(dir+"sobel")
    
    val lbp = original.localBinaryPatterns().toImage()
    lbp.toFile(dir+"local binary patterns")
    
    val coloredlbp = lbp.recolor{x:Pixel => Color.fromProbToHeat(x.color.v)}
    coloredlbp.toFile(dir+"local binary patterns (colored)")
    
    val kmeans = coloredlbp.kMeans(2).toImage()
    kmeans.toFile(dir+"k means")
    
    /*
     * TODO complete implementation 3d kernels
     */
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
  
  def maxPooling(arr:Array[Array[Int]]):Array[Array[Int]] = {
    ???
  }
}
