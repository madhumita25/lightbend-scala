package com.lightbend.training.scalatrain

sealed abstract class TrainInfo {
  def number:Int
}

case class InterCityExpress(number: Int, hasWiFi: Boolean = false) extends TrainInfo

case class RegionalExpress(number: Int) extends TrainInfo

case class BavarianRegional(number: Int) extends TrainInfo

case class Station(name: String)

case class Train(info: TrainInfo,
                 schedule: Seq[(Time, Station)]) {

  val stations: Seq[Station] = schedule.map(_._2)

  require(schedule.take(2).size == 2 , "Train should run between at least 2 stations")

  def timeAt (st: Station): Option[Time] = {
    schedule.toMap.map(_.swap).get(st)
  }
}
