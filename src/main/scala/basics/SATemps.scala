package basics;

case class TempRow(day: Int, doy: Int, month: Int, year: Int, precip: Double, tave: Double, tmax: Double, tmin: Double)

object SATemps {

  def parseLine(line: String): TempRow = {
    val p = line.split(",");
    TempRow(p(0).toInt,p(1).toInt,p(2).toInt,p(4).toInt,
            p(5).toDouble,p(6).toDouble,p(7).toDouble,p(8).toDouble);
  }

  def toMonth(n: Int): String = {
    n match {
      case 1 => "Jan";
      case 2 => "Feb";
      case 3 => "Mar";
      case 4 => "Apr";
      case 5 => "May";
      case 6 => "Jun";
      case 7 => "Jul";
      case 8 => "Aug";
      case 9 => "Sep";
      case 10 => "Oct";
      case 11 => "Nov";
      case 12 => "Dec";
    }
  }

  def main(args: Array[String]): Unit = {
    //--IMPORT DATA
    val source = scala.io.Source.fromFile("/users/mlewis/CSCI3395-F19/InClassBD/data/SanAntonioTemps.csv");
    val lines = source.getLines();
    //lines.drop(1).take(1).foreach(println);
    val data = lines.drop(2).map(parseLine).toArray;

    //--TEST DATA (didn't have access to file from home)
    /*val data: Array[TempRow] = Array(TempRow(1,1,1,2001,1.2,8.5,10.0,5.0),
                                     TempRow(2,2,1,2001,1.8,7.5,8.0,4.0),
                                     TempRow(3,3,1,2001,160.0,8.3,14,1.0),
                                     TempRow(1,32,2,2001,1.1,9.0,12.0,6.0),
                                     TempRow(2,33,2,2001,0.8,8.0,11.0,5.0),
                                     TempRow(1,51,3,2001,0.2,90.0,15.0,2.0));*/
    
    //----HIGHEST TEMPERATURE
    //val highTemp1 = data.maxBy(_.tmax);
    val highTemp2 = data.reduce((x, y) => if(x.tmax > y.tmax) x else y);
    //println(highTemp1);
    printf("Highest Temperature: %.1f on %d/%d/%d\n", highTemp2.tmax,highTemp2.day,highTemp2.month,highTemp2.year);
    
    //----HIGHEST PRECIPITATION
    val highPrecip = data.maxBy(_.precip);
    printf("Highest Precipitation: %.2f on %d/%d/%d\n",highPrecip.precip,highPrecip.month,highPrecip.month,highPrecip.year);
    
    //----FRACTION OF RAINY DAYS
    //val rainyDays = data.filter(_.precip > 1.0);
    //val rainyDays = data.count(_.precip > 1.0)
    val rainyDays: Int = data.foldLeft(0){ (sum, i) =>
      if(i.precip > 1.0)
        sum+1;
      else sum
    };
    printf("%.1f%% of days were rainy days\n", (rainyDays.toDouble/data.length)*100);

    //inclass
    val (rainySum, rainyCount) = data.foldLeft((0.0, 0)) {
      case ((sum, ct), day) =>
        if(day.precip > 1.0)
          (sum + day.precip, ct + 1)
        else (sum, ct)
    }
    val rainyAvgHigh = rainySum/rainyCount;

    //--ALL ATTEMPTS ON THE THREE FINAL QUESTIONS
    //val groupByMonth = data.groupBy(_.month);
    //val highTempAvgs = groupByMonth.keys.foreach(groupByMonth(_).maxBy(_.tmax));
    //val highTempAvgs: Array[TempRow] = Array();
    /*val JanData = data.filter(_.month == 1);
    val JanDataSize = JanData.length;

    val JanTMaxSum = JanData.foldLeft(0.0){ (sum, i) => sum + i.tmax };
    val JanTMaxAvg = JanTMaxSum/JanDataSize;
    printf("Avg High Temperature for January: %.1f \n", JanTMaxAvg);*/

    //highTempAvgs.foreach(i =>printf("Avg High Temperature for Month %d: %f \n", i.month, i.tmax));
  
    //--LAST THREE QUESTIONS
    val MonthsData = data.groupBy(_.month);
    println
    //----AVERAGE HIGHEST TEMPERATURE BY MONTH
    val MonthsDataTMaxAvg = MonthsData.mapValues(arr => arr.map(_.tmax).sum/arr.length);
    MonthsDataTMaxAvg.toSeq.sorted.foreach(x => printf("%s Avg Temperature High: %.1f\n", toMonth(x._1), x._2));
    println
    //----AVERAGE PRECIPITATION BY MONTH
    val MonthsDataPrecipAvg = MonthsData.mapValues(arr => arr.map(_.precip).sum/arr.length);
    MonthsDataPrecipAvg.toSeq.sorted.foreach(x => printf("%s Avg Precipitation: %.2f\n", toMonth(x._1), x._2));
    println
    //----MEDIAN PRECIPITATION BY MONTH
    val MonthsDataPrecipMed = MonthsData.mapValues(arr => arr.map(_.precip).sorted.apply(arr.length/2));
    MonthsDataPrecipMed.toSeq.sorted.foreach(x => printf("%s Median Precipitation: %.2f\n", toMonth(x._1), x._2));
  
  }

}