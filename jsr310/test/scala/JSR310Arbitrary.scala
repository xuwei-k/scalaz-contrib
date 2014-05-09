package scalaz.contrib
package jar310

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

import scalaz._
import scalaz.syntax.functor._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.std.AllInstances._

import java.time._

object JSR310Arbitrary {

  private def arb[A: Arbitrary] = implicitly[Arbitrary[A]]

  private val smallIntArb = Arbitrary(Gen.choose(1, 100000))

  implicit val DurationArbitrary: Arbitrary[Duration] =
    arb[Int] map { Duration.ofMillis(_) }

  implicit val PeriodArbitrary: Arbitrary[Period] =
    smallIntArb map { Period.ofDays(_) }

  implicit val LocalDateArbitrary: Arbitrary[LocalDate] =
    arb[Long] map { LocalDate.ofEpochDay }

  implicit val LocalTimeArbitrary: Arbitrary[LocalTime] =
    arb[Long] map { LocalTime.ofSecondOfDay }

  implicit val LocalDateTimeArbitrary: Arbitrary[LocalDateTime] =
    arb[Long] map { new LocalDateTime(_) }

  implicit val InstantArbitrary: Arbitrary[Instant] =
    arb[Long] map { new Instant(_) }

  implicit val YearMonthArbitrary: Arbitrary[YearMonth] =
    arb[Long] map { new YearMonth(_) }

  implicit val MonthDayArbitrary: Arbitrary[MonthDay] =
    arb[Long] map { new MonthDay(_) }

}
