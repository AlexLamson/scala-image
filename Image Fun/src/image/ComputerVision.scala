package image

import java.awt.image.{BufferedImage => BImage}
import java.awt.{Color => JColor}
import scala.collection.JavaConversions._
import java.awt.{Color => JColor}
import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.awt.image.{BufferedImage => BImage}

/*
 * Extends the Image class with CV-related methods and classes
 * 
 */

object ComputerVision {
  private def mod(a:Int, b:Int) = { val c = a % b; if(c < 0) c + b else c }
  
  implicit class RichImage(img:Image) {

    def sobel():SobelImage = new SobelImage(img)
    
    def canny():CannyImage = new CannyImage(img)
    
    //function f(clusters, movement, iterationNum)
    def kMeans(k:Int, f:((Array[Cluster], Int, Int)=>Unit)=(_,_,_)=>{}):KMeansImage = new KMeansImage(img, k, f)
    
    def localBinaryPatterns():LBPImage = new LBPImage(img)

    //add labels to an image
    def addLabels(stringsAndPoints:List[(String, (Int, Int))]): Image = {
      var newImg = img.copyBImage
      
      var g = newImg.getGraphics
      g.setColor(JColor.black)
      
      def drawString(text:String, x:Int, y:Int) = {
        import java.awt.Font
        import java.awt.font.FontRenderContext
        import java.awt.geom.AffineTransform
        
        val affinetransform = new AffineTransform()     
        val frc = new FontRenderContext(affinetransform, true, true)     
        val font = new Font("Tahoma", Font.PLAIN, 12)
        val textwidth = font.getStringBounds(text, frc).getWidth
        val textheight = font.getStringBounds(text, frc).getHeight
        
        def i(d:Double) = d.toInt
        
        val borderSize = 2
        val b = borderSize
        
        g.setColor(JColor.white)
        g.fillRect(i(x-textwidth/2.0)-b, i(y-textheight/2.0)-b, i(textwidth)+3+b*2, i(textheight)+b*2)
        g.setColor(JColor.black)
        g.drawString(text, i(x-textwidth/2.0), i(y+textheight/4.0))
      }
      
      for((string, point) <- stringsAndPoints) {
        drawString(string, point._1, point._2)
//        g.drawString(string, point._1, point._2)
      }
      
      new Image(newImg)
    }
    
    //apply average blur to image using given kernel size
    def blur(kWidth:Int=3) = {
      var newImg = img.copyBImage
      
      for(y <- 0 to img.height-1; x <- 0 to img.width-1){
        val kColors = for{
          y1 <- y-kWidth/2 to y+kWidth/2
          x1 <- x-kWidth/2 to x+kWidth/2
          y2 = mod(y1, img.height)
          x2 = mod(x1, img.width)
        } yield newImg.getRGB(x2, y2)
        
        val avgColor = Color.average(kColors.map(new Color(_)).toList)
        
        newImg.setRGB(x, y, avgColor.rgb)
      }
      new Image(newImg)
    }

    //normalize all the colors in the image
    //https://docs.gimp.org/en/plug-in-normalize.html
    def normalize() = {
      val minR = img.iterator.minBy{ _.r }.r
      val minG = img.iterator.minBy{ _.g }.g
      val minB = img.iterator.minBy{ _.b }.b
      val maxR = img.iterator.maxBy{ _.r }.r
      val maxG = img.iterator.maxBy{ _.g }.g
      val maxB = img.iterator.maxBy{ _.b }.b
      
      def norm(x:Int, min:Int, max:Int):Int = {
        val percentage = 1.0*(x-min)/(max-min)
        (255*percentage).toInt
      }
      
      def normColor(c:Color) = {
        val r = norm(c.r, minR, maxR)
        val g = norm(c.g, minG, maxG)
        val b = norm(c.b, minB, maxB)
        new Color(r, g, b)
      }
      
      img.recolor{x => normColor(x.color)}
    }
    
    //stretch out color histograms as much as possible
    //https://docs.gimp.org/en/gimp-layer-equalize.html
    def equalize():Image = {
      
      def makeEqualizeFunction(intensities:Array[Int]):(Int)=>Int = {
        val levels = 256
        
        val frequencies = {
          val freqMap = intensities.groupBy{ x => x }.mapValues { _.size }
          (0 until levels).map{ freqMap.getOrElse(_, 0) }.toArray
        }
        val cumulativeFrequencies = frequencies.scanLeft(0){ _ + _ }.tail.toArray
        val idealFrequencies = {
          val constant = cumulativeFrequencies.last/levels
          (0 until levels).map{ _ => constant }.toArray
        }
        val idealCumulativeFrequencies = idealFrequencies.scanLeft(0){ _ + _ }.tail.toArray
        
        val newIntensityMap = (0 until levels).map{ i =>
          val currentCumulativeFrequency = cumulativeFrequencies(i)
          idealCumulativeFrequencies.zipWithIndex.minBy{ case (cf, i) => math.abs(cf-currentCumulativeFrequency) }._2
        }
        
        i => newIntensityMap(i)
      }
      
      //make list of intensities for each color component
      val (rList, gList, bList) = img.iterator.map { _.toRgb }.toList.unzip3
      
      val rFunc = makeEqualizeFunction(rList.toArray)
      val gFunc = makeEqualizeFunction(gList.toArray)
      val bFunc = makeEqualizeFunction(bList.toArray)
      
      img.recolor { x => new Color(rFunc(x.color.r), gFunc(x.color.g), bFunc(x.color.b)) }
    }
    
    def getConnectedPixels(origin:Pixel, 
        f:(Pixel, Pixel)=>Boolean=(a,b)=>a.color==b.color, 
        compareToOrigin:Boolean=true, move8Ways:Boolean=false) = new Iterator[Pixel] {
      val processedArr = Array.fill(img.width, img.height)(false)
      var stack = new scala.collection.mutable.Stack[Pixel]()
      
      
      private def wasProcessed(pos:(Int, Int)) = processedArr(pos._1)(pos._2)
      private def markProcessed(pos:(Int, Int)) { processedArr(pos._1)(pos._2) = true }
      private def addTuples(a:(Int, Int), b:(Int, Int)) = (a._1+b._1, a._2+b._2)
      
      //if compareToOrigin is true, compare the new pixel to the first pixel given
      //otherwise, compare it to the pixel it is immediately connected to
      private def g(newPixel:Pixel, previousPixel:Pixel) = {
        if(compareToOrigin) f(newPixel, origin)
        else f(newPixel, previousPixel)
      }
      
      //given a previous pixel position and a delta, add the new pixel to the
      //stack if it hasn't already been processed & passes the function test
      private def addToStack(previousPixel:Pixel, delta:(Int, Int)) {
        val previousPos = previousPixel.pos
        val newPos = addTuples(previousPos, delta)
        if(img.inBounds(newPos._1, newPos._2) && !wasProcessed(newPos)) {
          val newPossiblePixel = img.getPixel(newPos._1, newPos._2)
          if(newPossiblePixel.nonEmpty) {
            val newPixel = newPossiblePixel.get
            if(g(newPixel, previousPixel)) {
              stack = stack.push(newPixel)
            }
            markProcessed(newPos)
          }
        }
      }
      
      //push initial pixel to the stack
      addToStack(origin, (0, 0))
      
      val w = (-1, 0)
      val n = (0, 1)
      val e = (1, 0)
      val s = (0, -1)
      
      val nw = (-1, 1)
      val ne = (1, 1)
      val se = (1, -1)
      val sw = (-1, -1)
      
      //required iterator methods
      def hasNext = !stack.isEmpty
      def next = {
        val previousPixel = stack.pop
        
        addToStack(previousPixel, w)
        addToStack(previousPixel, n)
        addToStack(previousPixel, e)
        addToStack(previousPixel, s)
        if(move8Ways) {
          addToStack(previousPixel, nw)
          addToStack(previousPixel, ne)
          addToStack(previousPixel, se)
          addToStack(previousPixel, sw)
        }
        
        previousPixel
      }
    }
    
  } //end of implicit class
  
  object Cluster {
    //render an array of clusters to an image
    def clustersToImage(clusters:Array[Cluster]):Image = {
      val (w, h) = clusters.foldLeft((0, 0)){ (currMax, cluster) =>
        val clusterMax = cluster.points.foldLeft((0, 0)){ (a, b) =>
          (math.max(a._1, b._1), math.max(a._2, b._2))
        }
        (math.max(currMax._1, clusterMax._1), math.max(currMax._2, clusterMax._2))
      }
      
      val outputImg = new Image(w+1, h+1)
      for(cluster <- clusters; p <- cluster.points) {
        if(p._1 >= outputImg.width || p._2 >= outputImg.height)
          println("img:"+outputImg+"p:"+p)
        outputImg.img.setRGB(p._1, p._2, cluster.color.rgb)
      }
      outputImg
    }
  }
  
  //cluster represents a k-means cluster, containing points and a centroid
  class Cluster(img:Image) {
    var pos = newPoint()
    var color = img.get(pos._1, pos._2).get
    
    var points = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
    
    def distance(x:Int, y:Int):Int = {
      val c = img.get(x, y).get
      val r = c.r
      val g = c.g
      val b = c.b
      
      val px = pos._1
      val py = pos._2
      val pc = color
      val pr = pc.r
      val pg = pc.g
      val pb = pc.b
      
      def sq(i:Int) = i*i
      val spaceDist = (sq(255*x/img.width-255*px/img.width) +
                      sq(255*y/img.height-255*py/img.height))/2.0
      val colorDist = (sq(r-pr) +
                      sq(g-pg) +
                      sq(b-pb))/3.0
      val dist = (1.0*spaceDist) + (0.5*colorDist)
      
//      println("("+x+", "+y+") vs ("+px+", "+py+")")
//      println("("+r+", "+g+", "+b+") vs ("+pr+", "+pg+", "+pb+")")
//      println(c+" vs "+pc)
//      println("   spaceDist: "+spaceDist)
//      println("   colorDist: "+colorDist)
      
      dist.toInt
    }
    
    def size() = points.length
    
    private def newPoint() = ((img.width*math.random).toInt, (img.height*math.random).toInt) 
  }
  
  //class represents an image broken into k clusters
  class KMeansImage(img:Image, k:Int, f:((Array[Cluster], Int, Int)=>Unit)=(_,_,_)=>{}) {
    val clusters = kMeans(k, f)
    
    //k means
    //function f(clusters, movement, iterationNum)
    private def kMeans(k:Int, f:((Array[Cluster], Int, Int)=>Unit)=(_,_,_)=>{}):List[Cluster] = {
      //initalize the clusters
      val clusters = (1 to k).map(_ => new Cluster(img)).toArray
      
      //give each cluster a bunch of random pixels
      val iterator = img.iteratorPositions
      var counter = 0
      val totalPixels = img.width*img.height
      while(iterator.hasNext) { //for each pixel
        val (x, y) = iterator.next()
        val clusterNum = (k * counter / totalPixels)
        
        clusters(clusterNum).points += ((x, y))
        
        counter += 1
      }
      
      //repeat until no change
      var changeMade = true
      var iterationNum = 0
      while(changeMade) {
        changeMade = false
        
        //  for every pixel (in each cluster), assign it to the cluster with the closest centroid
        for(cluster <- clusters) {
          
          for(i <- cluster.points.length-1 to 0 by -1) {
            val p = cluster.points(i)
            val (c, newI) = clusters.zipWithIndex.minBy{c => c._1.distance(p._1, p._2)}
            if(i != newI) {
              clusters(newI).points += ((p._1, p._2)) //add to closer cluster
              cluster.points.remove(i) //remove from old cluster
            }
          }
        }
        
        //  for every centroid, move that centroid to the average of its points
        var movement = 0 //measures convergence
        for(cluster <- clusters) {
          val px = cluster.pos._1
          val py = cluster.pos._2
          val r = cluster.color.r
          val g = cluster.color.g
          val b = cluster.color.b
          
          def  avg(sum:Int, n:Int) = if(n != 0) sum/n else 0
          
          //calc average position
          val sumPoint = cluster.points.foldLeft((0, 0)){(a,b) => (a._1+b._1, a._2+b._2)}
          val numPoints = cluster.points.length
          val avgX = avg(sumPoint._1, numPoints)
          val avgY = avg(sumPoint._2, numPoints)
          
          //calc average color
          val avgColor = Color.average(cluster.points.map(p => img.get(p._1, p._2).get).toList)
          val avgR = avgColor.r
          val avgG = avgColor.g
          val avgB = avgColor.b
          
          if(px != avgX || py != avgY || r != avgR || g != avgG || b != avgB) {
            changeMade = true
            
            //track how "converged" the algorithm is
            def sq(i:Int) = i*i
            val c = img.get(cluster.pos._1, cluster.pos._2).get
            val pc = img.get(avgX, avgY).get
            val moves = sq(255*(cluster.pos._1-avgX)/img.width) +
                        sq(255*(cluster.pos._2-avgX)/img.height) +
                        sq(c.r-pc.r) +
                        sq(c.g-pc.g) +
                        sq(c.b-pc.b)
            movement += moves
            
            //move the centroid to its new average
            cluster.pos = (avgX, avgY)
            cluster.color = avgColor
          }
          
        }
        
        iterationNum += 1
        
        f(clusters, movement, iterationNum)
        
      } //while changeMade == true
      
      clusters.toList
    }
    
    def toImage():Image = makeImage(_.color)
    
    def toImageRandomColors():Image = makeImage(_ => Color.random)
    
    private def makeImage(f:(Cluster)=>Color):Image = {
      val outputImg = new Image(img.width, img.height)
      for(cluster <- clusters) {
        for(p <- cluster.points) {
          outputImg.img.setRGB(p._1, p._2, f(cluster).rgb)
        }
      }
      outputImg
    }
  } //end of KMeansImage class
  
  class LBPImage(img:Image) {
    val arr = calculateLBP(img)
    
    //calculate byte value for every pixel using local binary patterns algorithm
    private def calculateLBP(img:Image, patternRadius:Int=1):Array[Array[Int]] = {
      val p = patternRadius
      val arr = img.to2dIntArray
      var outArr = img.to2dIntArray
      
      for(x <- p to arr(0).length-1-p) {
        for(y <- p to arr.length-1-p) {
          def thres(c:Int) = if(c < arr(y)(x)) 0 else 1
          def bit(x1:Int, y1:Int) = thres(arr(y+y1)(x+x1))
          
          val byte = 
            ((bit(-p, -p) << 7) |
            (bit(0, -p) << 6) |
            (bit(p, -p) << 5) |
            (bit(-p,0) << 4) |
            (bit(p, 0) << 3) |
            (bit(-p, p) << 2) |
            (bit(0, p) << p) |
            (bit(p, p))).toInt
          
          def bound(x:Int) = if(x < 0) 0 else if(x > 255) 255 else x
          outArr(y)(x) = bound(byte)
        }
      }
      
      outArr
    }
    
    def getBinCounts():Array[Int] = {
      val featureImage = Image.from2dIntArray(arr)
      var patternCounts = Array.fill(256){0}
      for(color <- featureImage.iterator) {
        patternCounts(color.r) += 1
      }
      patternCounts
    }
    
    //normalized counts
    def getBinPercentages():Array[Double] = {
      val binCounts = getBinCounts()
      val binSum = binCounts.sum
      binCounts.map{ 1.0 * _ / binSum }
    }
  
    def toImage() = Image.from2dIntArray(arr)
  
    def toImageHistogram() = Image.histogram(512, 512, getBinCounts())
  } //end of LBPImage class
  
  //class representing an image passed through a Sobel filter
  class SobelImage(img:Image) {
    
    assert(img.width != 0 && img.height != 0)
    val (width, height) = (img.width, img.height)
    
    val (gxArr, gyArr, gradArr, angleArr) = {
      val kernelX = Array(
        Array(-1, -2, -1),
        Array(0, 0, 0),
        Array(1, 2, 1)
      )
      
      val kernelY = Array(
        Array(-1, 0, 1),
        Array(-2, 0, 2),
        Array(-1, 0, 1)
      )
      
      def applyKernel(a:Array[Array[Int]], b:Array[Array[Int]], x1:Int, y1:Int):Int = {
        var sum = 0
        for(i <- 0 to a.length-1)
          for(j <- 0 to a(0).length-1){
            
            val q = a(i)(j)
            val xPos = mod(x1+j-a(0).length/2, img.width-1)
            val yPos = mod(y1+i-a.length/2, img.height-1)
            val w = b(yPos)(xPos)
            sum += q*w
          }
        sum
      }
      
      val imageArray = img.to2dIntArray
      
      var GxArr = Array.fill(img.width, img.height)(0)
      var GyArr = Array.fill(img.width, img.height)(0)
      var gradArr = Array.fill(img.width, img.height)(0.0)
      var angleArr = Array.fill(img.width, img.height)(0.0)
      
      for(y <- 0 to img.height-1){
        for(x <- 0 to img.width-1){
          val Gx = applyKernel(kernelX, imageArray, x, y)
          val Gy = applyKernel(kernelY, imageArray, x, y)
          
          val G = math.abs(Gx) + math.abs(Gy)
          
          val consG = if(G > 255) 255 else if(G < 0) 0 else G 
          
          //calculate gradient magnitude (can be thought of "edginess")
          val gradient = 1.0*consG/255
          
          val angle = math.atan2(Gy, Gx).toDegrees+180.0 //[0, 360]
          
          GxArr(x)(y) = Gx
          GyArr(x)(y) = Gy
          gradArr(x)(y) = gradient
          angleArr(x)(y) = angle
          
        }
      }
      
      (GxArr, GyArr, gradArr, angleArr)
    }
    
    def canny():CannyImage = new CannyImage(this)
    
    def toImage(colored:Boolean=false):Image = {
      var newImg = new BImage(img.width, img.height, BImage.TYPE_INT_RGB)
      for(y <- 0 to img.height-1){
        for(x <- 0 to img.width-1){
          val sat = if(colored) 1 else 0
          val angle = (angleArr(x)(y)/360.0f).toFloat
          assert(angle >= 0 && angle <= 1, "sobelImage.toImage angle out of bounds: "+angle+" unfiltered: "+angleArr(x)(y))
//          println("sobelImage.toImage angle: "+angle+" unfiltered: "+angleArr(x)(y))
          val edginess = gradArr(x)(y).toFloat
          val color = Color.fromHsv(angle, sat, edginess)
          newImg.setRGB(x, y, color.rgb)
        }
      }
      new Image(newImg)
    }
    
    def toImageColored():Image = toImage(true)
  } //end of SobelImage class
  
  class CannyImage(val sobelImg:SobelImage) {
    def this(img:Image) = this(new SobelImage(img))
    
    assert(sobelImg.width != 0 && sobelImg.height != 0)
    val (width, height) = (sobelImg.width, sobelImg.height)
    
    //gxArr, gyArr, gradArr, angleArr
    
    //set every point that is not a local max in its edge direction to 0
    val nonMaximumSuppressedArr:Array[Array[Double]] = {
      
      //copy the gradient array
      val suppresedGradArr = sobelImg.gradArr.transpose.transpose
      
      //round angle a to be either 0, 45, 90 or 135 degrees
      def roundAngle(a:Double):Double = {
        val angle = if(a >= 180) a-180 else a
        
        //return true if b <= a <= c
        def isBetween(a:Double, b:Double, c:Double) = (b <= a && a <= c)
        def avg(a:Double, b:Double) = (a+b)/2.0
        
        if(isBetween(angle, avg(90, 135), avg(135, 180))) 135
        else if(isBetween(angle, avg(45, 90), avg(90, 135))) 90
        else if(isBetween(angle, avg(0, 45), avg(45, 90))) 45
        else 0
      }
      
      for(y <- 0 to sobelImg.height-1; x <- 0 to sobelImg.width-1) {
        val angle = roundAngle(sobelImg.angleArr(x)(y))
        val edginess = sobelImg.gradArr(x)(y)
        
//        def inBounds(x:Int, y:Int) = (x >= 0 && x < width && y >= 0 && y < height)
//        
//        def getGradient(x:Int, y:Int) = sobelImg.gradArr(x)(y)
//        
//        def suppress(angleDelta:Int) {
//          val deltaX = math.round(math.cos((angle+angleDelta).toRadians)).toInt
//          val deltaY = math.round(math.sin((angle+angleDelta).toRadians)).toInt
//          
////          println(angle.toInt+" => delta: "+(deltaX, deltaY))
//          
//          if(inBounds(x+deltaX, y+deltaY)) {
//            val deltaGrad = getGradient(x+deltaX, y+deltaY)
//            val myGrad = getGradient(x, y)
//            if(deltaGrad < myGrad) {
//              suppresedGradArr(x+deltaX)(y+deltaY) = 0
//            }
//          }
//        }
//        
//        if(edginess != 0) {
//          suppress(-90)
//          suppress(90)
//        }
      }
      
      suppresedGradArr
    }
    
//    //set all gradients < minVal to 0
//    //set all gradients < minVal to 1
//    //set all pixels in between to 1 if they are connected to other 1's 
//    val hysteresisThresholdedArr:Array[Array[Int]] = {
//      val minVal = 0 //everything above this COULD be an edge
//      val maxVal = 0 //everything above this MUST be an edge
//      
//      // operates on nonMaximumSuppressedArr
//      
//      ???
//    }
    
    def toImage():Image = {
//      val floatArr = hysteresisThresholdedArr.map{_.map{_.toFloat}}
      
//      val floatArr = nonMaximumSuppressedArr.map{_.map{_.toFloat}}
//      Image.from2dFloatArray(floatArr.transpose)
      
      sobelImg.toImage(colored=true)
    }
  }
  
} //end of object
