package fun

import image._
import image.ComputerVision._
import java.awt.Graphics

object Fun11 {
  var counter = 0
  def dir() = { counter += 1; "res/output/Screen time/"+counter }
  type Dub = Double
  def cos(x:Dub) = math.cos(x)
  def sin(x:Dub) = math.sin(x)
  val pi = math.Pi
  def i(d:Dub) = d.toInt
  def rand(min:Double, max:Double) = math.random*(max-min)+min
  def sq(x:Dub) = x*x
  def dist(x1:Double, y1:Double, x2:Double, y2:Double) = math.sqrt(sq(x1-x2)+sq(y1-y2))
  
  def main(args: Array[String]) = {
    println("new program starting")
    val startTime = System.currentTimeMillis
    
    
//    import java.text.SimpleDateFormat
//    import java.util.Date
//    
//    val timeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
//    def parseLine(line:String) = {
//      val lineParts = line.split(" - ")
//      val timestamp = timeFormat.parse(lineParts(0))
//      val screenOn = (lineParts(1) == "Screen On")
//      val charging = (lineParts(2) == "Unplugged")
//      (timestamp, screenOn, charging)
//    }
//    
//    val lines = File.readLines("res/output/Screen Time/Log1.txt").toArray
//    
//    val secondsInDay = (24*60*60)
//    val w = secondsInDay/100
//    
//    val startDate = parseLine(lines.head)._1
//    val endDate = parseLine(lines.last)._1
//    println("startDate: "+startDate)
//    println("endDate: "+endDate)
//    
//    val h = {
//      val days = (endDate.getTime - startDate.getTime) / 1000 / secondsInDay
//      days.toInt
//    } //total number of days
//    
//    println("Image will be "+w+" x "+h)
//    
//    
//    def getSecondsPassedToday(timestamp:Date) = (timestamp.getTime % secondsInDay).toInt
//    
//    def getMidnight(timestamp:Date):Date = {
//      val time = timestamp.getTime()
//      new Date(time - time % (24 * 60 * 60 * 1000))
//    }
//    
//    val startDateDay = getMidnight(startDate)
//    def getDayNum(timestamp:Date):Int = {
//      ( (getMidnight(timestamp).getTime - startDateDay.getTime) / 1000 / secondsInDay ).toInt
//    }
//    
//    
//    
//    def chooseColor(screenOn:Boolean, charging:Boolean, g:Graphics) {
//      if(screenOn && !charging) g.setColor(Color.red.color)         //red    = using phone
//      else if(screenOn && charging) g.setColor(Color.yellow.color)  //yellow = in bed using phone
//      else if(!screenOn && !charging) g.setColor(Color.green.color) //green  = walking
//      else if(!screenOn && charging) g.setColor(Color.blue.color)   //blue   = sleeping
//    }
//    
//    val img = Image.generate(w, h, { g:Graphics =>
//      for(i <- 0 to lines.length-2) {
//        val (timestamp, screenOn, charging) = parseLine(lines(i))
//        chooseColor(screenOn, charging, g)
//        
//        val dayNum = getDayNum(timestamp)
//        
//        val secondsNow = getSecondsPassedToday(timestamp)/100
//        val secondsNext = getSecondsPassedToday(parseLine(lines(i+1))._1)/100
//        
//        //typical case
//        if(secondsNow <= secondsNext) {
//          g.drawLine(secondsNow, dayNum, secondsNext, dayNum)
//        }
//        //when overlapping midnight
//        else {
//          g.drawLine(secondsNow, dayNum, secondsInDay-1, dayNum)
//          g.drawLine(0, dayNum+1, secondsNext, dayNum+1)
//        }
//      }
//    })
//    img.toFile(dir+" phone awakeness")
    import java.text.SimpleDateFormat
    import java.util.Date
    
    val timeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    def parseLine(line:String) = {
      val lineParts = line.split(" - ")
      val timestamp = timeFormat.parse(lineParts(0))
      val screenOn = (lineParts(1) == "Screen On")
      val charging = (lineParts(2) == "Unplugged")
      (timestamp, screenOn, charging)
    }
    
    val lines = File.readLines("res/output/Screen Time/Log1.txt").toArray
    
    val secondsInDay = (24*60*60)
    val w = secondsInDay/10
    
    val startDate = parseLine(lines.head)._1
    val endDate = parseLine(lines.last)._1
    println("startDate: "+startDate)
    println("endDate: "+endDate)
    
    val dayHeight = 20
    
    val h = {
      val days = (endDate.getTime - startDate.getTime) / 1000 / secondsInDay
      days.toInt
    }*dayHeight //total number of days
    
    println("Image will be "+w+" x "+h)
    
    
    def getSecondsPassedToday(timestamp:Date) = ((timestamp.getTime / 1000) % secondsInDay).toInt
    
    def getMidnight(timestamp:Date):Date = {
      val time = timestamp.getTime()
      new Date(time - time % (24 * 60 * 60 * 1000))
    }
    
    val startDateDay = getMidnight(startDate)
    def getDayNum(timestamp:Date):Int = {
      ( (getMidnight(timestamp).getTime - startDateDay.getTime) / 1000 / secondsInDay ).toInt
    }
    
    
    
    def chooseColor(screenOn:Boolean, charging:Boolean, g:Graphics) {
      if(screenOn && !charging) g.setColor(Color.red.color)         //red    = using phone
      else if(screenOn && charging) g.setColor(Color.yellow.color)  //yellow = in bed using phone
      else if(!screenOn && !charging) g.setColor(Color.green.color) //green  = walking
      else if(!screenOn && charging) g.setColor(Color.blue.color)   //blue   = sleeping
    }
    
    val img = Image.generate(w, h, { g:Graphics =>
      for(i <- 0 to lines.length-2) {
        val (timestamp, screenOn, charging) = parseLine(lines(i))
        chooseColor(screenOn, charging, g)
        
        val dayNum = getDayNum(timestamp)*dayHeight
        
        val secondsNow = getSecondsPassedToday(timestamp)/10
        val secondsNext = getSecondsPassedToday(parseLine(lines(i+1))._1)/10
        
        //typical case
        if(secondsNow <= secondsNext) {
//          g.drawLine(secondsNow, dayNum, secondsNext, dayNum)
          g.fillRect(secondsNow, dayNum, secondsNext-secondsNow, dayHeight)
        }
        //when overlapping midnight
        else {
//          g.drawLine(secondsNow, dayNum, secondsInDay-1, dayNum)
//          g.drawLine(0, dayNum+1, secondsNext, dayNum+1)
          g.fillRect(secondsNow, dayNum, secondsInDay-1-secondsNow, dayHeight)
          g.fillRect(0, dayNum+1, secondsNext-0, dayHeight)
        }
      }
    })
    img.toFile(dir+" phone awakeness")
    
    
    
    val endTime = System.currentTimeMillis
    val seconds = "%.3f".format((endTime-startTime)/1000.0)
    println("program finished in "+seconds+" s")
  }
}
