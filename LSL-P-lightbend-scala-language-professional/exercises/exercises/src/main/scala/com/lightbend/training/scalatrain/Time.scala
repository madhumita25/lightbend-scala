package com.lightbend.training.scalatrain

import play.api.libs.json.{JsNumber, JsObject, JsValue}

import scala.math.Ordered
import scala.util.{Try, Success, Failure}

object Time {
  def fromMinutes(minutes: Int) = Time(minutes/60, minutes%60)

  def fromJson( js: JsValue): Option[Time] = {
    for {
      hours <- Try(( js\ "hours").as[Int])
      minutes <- Try(( js\ "minutes").as[Int]) match {
        case Success(value) => Success(value)
        case Failure(_) => Success(0)
      }
    } yield Time(hours, minutes)
  }.toOption
}


case class Time(hours: Int = 0,
                minutes: Int = 0) extends Ordered[Time] {


  require(hours >= 0 && hours < 24, "Hours should be within 0 and 23")
  require(minutes >= 0 && minutes < 60, "Minutes should be within 0 and 59")

  val asMinutes = hours * 60 + minutes

  def minus(that: Time) = this.asMinutes - that.asMinutes

  def -(that: Time) = minus(that)

  override def toString = f"$hours%02d:$minutes%02d"

  def compare(that: Time) = this.asMinutes - that.asMinutes

  def toJson : JsValue = JsObject(
    Map("hours" -> JsNumber(hours), "minutes" -> JsNumber(minutes))
    )

}



