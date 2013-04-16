package scalaz.contrib
package nscala_time

import scalaz._
import org.joda.time._

trait LocalDateTimeInstances{
  implicit val localDateTimeInstance = new Order[LocalDateTime]{
    def order(x: LocalDateTime, y: LocalDateTime) = Ordering.fromInt(x compareTo y)
  }
}
