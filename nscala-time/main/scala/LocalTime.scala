package scalaz.contrib
package nscala_time

import scalaz._
import org.joda.time._

trait LocalTimeInstances{
  implicit val localTimeInstance = new Order[LocalTime]{
    def order(x: LocalTime, y: LocalTime) = Ordering.fromInt(x compareTo y)
  }
}
