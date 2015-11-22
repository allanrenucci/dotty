
import scala.annotation.Idempotent


class Experiment {

  //@Idempotent def idem(a: Int, b: Int) = a + b

  def foo(cond: Boolean): Unit = {
    if (cond) 1 else 0
    if (cond) 1
  }

}
