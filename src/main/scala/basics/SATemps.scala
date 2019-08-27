package basics;

case class TempRow(day: Int, doy: Int, month: Int, year: Int, precip: Double, tave: Double, tmax: Double, tmin: Double)

object SATemps {

  def parseLine(line: String): TempRow = {
    val p = line.split(",");
    TempRow(p(0).toInt,p(1).toInt,p(2).toInt,p(4).toInt,
            p(5).toDouble,p(6).toDouble,p(7).toDouble,p(8).toDouble);
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("/users/mlewis/CSCI3395-F19/InClassBD/data/SanAntonioTemps.csv");
    val lines = source.getLines();
    lines.drop(1).take(1).foreach(println);
    val data = lines.drop(2).map(parseLine).toArray;

    val highTemp1 = data.maxBy(_.tmax);
    val highTemp2 = data.reduce((x, y) => if(x.tmax > y.tmax) x else y);
    println(highTemp1);
    println(highTemp2);

    val highPrecip = data.maxBy(_.precip);
    println(highPrecip);

    //val rainyDays = data.filter(_.precip > 1.0);
    val rainyDays = data.count(_.precip > 1.0)
    printf("%.3f\n", rainyDays.toDouble/data.length);
  }

}