package basics;

case class TempRow(day: Int, doy: Int, month: Int, year: Int, precip: Double, tave: Double, tmax: Double, tmin: Double)

object SATemps {

  def parseLine(line: String): TempRow = {
    val p = line.split(",");
    TempRow(p(0).toInt,p(1).toInt,p(2).toInt,p(4).toInt,
            p(5).toDouble,p(6).toDouble,p(7).toDouble,p(8).toDouble);
  }

  def main(args: Array[String]): Unit = {
    /*val source = scala.io.Source.fromFile("/users/mlewis/CSCI3395-F19/InClassBD/data/SanAntonioTemps.csv");
    val lines = source.getLines();
    lines.drop(1).take(1).foreach(println);
    val data = lines.drop(2).map(parseLine).toArray;*/
    val data: Array[TempRow] = Array(TempRow(1,1,1,2001,1.2,8.5,10.0,5.0),
                                     TempRow(2,2,1,2001,1.5,7.5,8.0,4.0),
                                     TempRow(1,32,2,2001,1.1,9.0,12.0,6.0),
                                     TempRow(2,33,2,2001,0.8,8.0,11.0,5.0));
    //----HIGHEST TEMPERATURE
    //val highTemp1 = data.maxBy(_.tmax);
    val highTemp2 = data.reduce((x, y) => if(x.tmax > y.tmax) x else y);
    //println(highTemp1);
    printf("Highest Temperature: %s\n", highTemp2);
    //----HIGHEST PRECIPITATION
    val highPrecip = data.maxBy(_.precip);
    printf("Highest Precipitation: %s\n",highPrecip);
    //----FRACTION OF RAINY DAYS
    //val rainyDays = data.filter(_.precip > 1.0);
    //val rainyDays = data.count(_.precip > 1.0)
    val rainyDays: Int = data.foldLeft(0){ (sum, i) =>
      if(i.precip > 1.0)
        sum+1;
      else sum
    };
    printf("%.1f%% of days were rainy days\n", (rainyDays.toDouble/data.length)*100);

    //val groupByMonth = data.groupBy(_.month);
    //val highTempAvgs = groupByMonth.keys.foreach(groupByMonth(_).maxBy(_.tmax));
    //val highTempAvgs: Array[TempRow] = Array();
    val JanData = data.filter(_.month == 1);
    val JanDataSize = JanData.length;

    val JanTMaxSum = JanData.foldLeft(0.0){ (sum, i) => sum + i.tmax };
    val JanTMaxAvg = JanTMaxSum/JanDataSize;
    printf("Avg High Temperature for January: %.1f \n", JanTMaxAvg);
    //highTempAvgs.foreach(i =>printf("Avg High Temperature for Month %d: %f \n", i.month, i.tmax));
  }

}