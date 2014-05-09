package scalaz.contrib
package jsr310

import scalaz._
import scalaz.syntax.enum._

import java.time._

trait Instances {
  private def orderFromInt[A](f: (A, A) => Int): Order[A] = new Order[A] {
    def order(x: A, y: A) = Ordering.fromInt(f(x, y))
  }

  implicit val durationInstance = new Monoid[Duration] with Order[Duration] {
    override def zero = Duration.ZERO
    override def append(f1: Duration, f2: ⇒ Duration) = f1 plus f2
    override def order(a1: Duration, a2: Duration) = Ordering.fromInt(a1 compareTo a2)
  }

  implicit val periodInstance = new Monoid[Period] with Equal[Period] {
    override val zero = Period.ZERO
    override def append(f1: Period, f2: ⇒ Period) = f1 plus f2
    override def equal(a1: Period, a2: Period) = a1 == a2
  }

  implicit val yearMonthInstance     = orderFromInt[YearMonth](_ compareTo _)
  implicit val monthDayInstance      = orderFromInt[MonthDay](_ compareTo _)
  implicit val instantInstance       = orderFromInt[Instant](_ compareTo _)
  implicit val localTimeInstance     = orderFromInt[LocalTime](_ compareTo _)

  implicit val localDateInstance = new Enum[LocalDate] {
    override def order(x: LocalDate, y: LocalDate): Ordering =
      Ordering.fromInt(x compareTo y)
    override def pred(a: LocalDate): LocalDate = a.minusDays(1)
    override def succ(a: LocalDate): LocalDate = a.plusDays(1)
  }

  implicit val localDateTimeInstance = new Enum[LocalDateTime] {
    override def order(x: LocalDateTime, y: LocalDateTime): Ordering =
      Ordering.fromInt(x compareTo y)
    override def pred(a: LocalDateTime): LocalDateTime = a.minusDays(1)
    override def succ(a: LocalDateTime): LocalDateTime = a.plusDays(1)
  }
}

// vim: expandtab:ts=2:sw=2
