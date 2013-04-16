package scalaz.contrib
package nscala_time

import scalaz._
import org.joda.time.DateTime

trait DateTimeInstances{
  implicit val dateTimeInstance = new Order[DateTime]{
    def order(x: DateTime, y: DateTime) = Ordering.fromInt(x compareTo y)
  }
}
