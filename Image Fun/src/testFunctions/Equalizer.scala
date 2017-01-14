package testFunctions

import image.ComputerVision._
import image.Image
import image.Color
import image.File

object Equalizer {
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    println("reading file")
    val original = File.readImage("res/testing/3.jpg").constrainTo(1000, 1000)
    val trueUntinted = File.readImage("res/testing/4.jpg").constrainTo(1000, 1000)
    val trueUntinted2 = File.readImage("res/testing/5.jpg").constrainTo(1000, 1000)
    
    val avg = original.average
//    val invertedAvg = avg.invert()
    val invertedAvg = new Color(0, 206, 255)
    
    val colors = (1 to 100*100).map{_ => invertedAvg}.toList
    val averageColorImg = Image.fromColorList(colors)
    averageColorImg.toFile("res/output/0 - average")
    
//    val untintedImg = original.recolor { _ - (avg * 0.5) }
    val untintedImg = original.recolor { x => x.color.blendIn(invertedAvg, 0.25) }
    
    
//    weighted average of hue
    
    
    original.histogramValues(x=>(x.v*255).toInt).toFile("res/output/v1 - original values histogram")
    untintedImg.histogramValues(x=>(x.v*255).toInt).toFile("res/output/v2 - untinted values histogram")
    
    
    
    original.histogramRGB().toFile("res/output/h1 - original histogram")
    untintedImg.histogramRGB().toFile("res/output/h2 - untinted histogram")
    trueUntinted.histogramRGB().toFile("res/output/h3 - true untinted histogram")
    trueUntinted2.histogramRGB().toFile("res/output/h3 - true untinted 2 histogram")
    
    println("writing output image(s)")
    original.toFile("res/output/1 - original")
    untintedImg.toFile("res/output/2 - untinted")
    
    val endTime = System.currentTimeMillis
    println("program finished in "+(endTime-startTime)+" ms")
  }
}
