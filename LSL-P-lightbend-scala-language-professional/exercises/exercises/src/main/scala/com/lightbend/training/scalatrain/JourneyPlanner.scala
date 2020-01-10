package com.lightbend.training.scalatrain

class JourneyPlanner (trains: Set[Train]) {
  val stations: Set[Station] = trains.flatMap(_.stations)

  def trainsAt(s: Station): Set[Train] = trains.filter(_.stations.contains(s))

  /*
  def stopsAt(s: Station): Set[(Time, Train)] = {
    for {
      t <- trains
      (time,st) <- t.schedule if st == s
    } yield (time, t)
  }
  */

  def stopsAt(s: Station): Set[(Time, Train)] = {
    for {
      t <- trains
      time <- t.timeAt(s)
    } yield (time, t)
  }

  def isShortTrip(from: Station, to:Station): Boolean = {

    /*.
    trains.exists( t =>
      t.stations.dropWhile( s => s != from).drop(1).take(2).contains(to)
    )*/

    trains.exists( t =>
      t.stations.dropWhile( s => s != from) match {
        case from +: st +: `to` +: _ => true
        case from +: `to` +: _ => true
        case _ => false
      }
    )

  }
}
