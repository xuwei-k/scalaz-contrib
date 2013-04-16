package scalaz.contrib
package nscala_time

import scalaz._
import org.joda.time._

trait LocalDateInstances{
  implicit val localDateInstance = new Order[LocalDate]{
    def order(x: LocalDate, y: LocalDate) = Ordering.fromInt(x compareTo y)
  }
}
