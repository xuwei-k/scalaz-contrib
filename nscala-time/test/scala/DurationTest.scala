package scalaz.contrib

import org.specs2.scalaz.Spec
import scalaz.scalacheck.ScalazProperties._
import scalaz.contrib.NScalaTimeArbitrary._
import org.joda.time._
import scalaz.contrib.nscala_time._

class DurationTest extends Spec {

  checkAll(monoid.laws[Duration])
  checkAll(equal.laws[Duration])

}
