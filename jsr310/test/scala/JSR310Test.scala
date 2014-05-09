package scalaz.contrib
package jsr310

import org.specs2.scalaz.Spec

import scalaz.scalacheck.ScalazProperties._

import java.time._

class JSR310Test extends Spec {

  import JSR310Arbitrary._

  checkAll("DateTime", enum.laws[DateTime])

  checkAll("Duration", monoid.laws[Duration])
  checkAll("Duration", order.laws[Duration])

  checkAll("Instant", order.laws[Instant])

  checkAll("Interval", equal.laws[Interval])

  checkAll("LocalDate", enum.laws[LocalDate])

  checkAll("LocalDateTime", enum.laws[LocalDateTime])

  checkAll("LocalTime", order.laws[LocalTime])

  checkAll("MonthDay", order.laws[MonthDay])

  checkAll("Months", monoid.laws[Months])
  checkAll("Months", order.laws[Months])

  checkAll("Period", monoid.laws[Period])
  checkAll("Period", equal.laws[Period])

  checkAll("YearMonth", order.laws[YearMonth])

}
