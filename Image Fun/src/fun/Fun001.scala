package fun

import image._

object Fun1 {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    var counter = 0
    def dir() = { counter += 1; "res/output/squares/"+counter+" - " }
    
    
    println("reading file")
    val original = File.readImage("res/training/negative/3.jpg") //bedroom
    original.toFile(dir+"original")
    
    val copyImg = new Image(original.width, original.height)
    val g = copyImg.g
    val minSize = math.min(copyImg.width, copyImg.height)
    
    def randomPos = ((math.random*copyImg.width).toInt, (math.random*copyImg.height).toInt)
    for(rectSize <- minSize/2 to 1 by -1) {
      
      for(i <- 1 to (copyImg.width/rectSize)*(copyImg.height/rectSize)) {
        val (x, y) = randomPos
        g.setColor(original.get(x, y).get.color)
        g.fillRect(x-rectSize/2, y-rectSize/2, rectSize, rectSize)
      }
      
      copyImg.toFile(dir+"squares")
    }
    
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
