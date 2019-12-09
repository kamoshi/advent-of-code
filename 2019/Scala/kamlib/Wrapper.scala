package kamlib

/** Wraps a function, measures the execution time, provides the result and time*/
class Wrapper[A](function: => A)
{
  /** Saved results */
  val tuple: (A, Double) = wrap(function)

  /** Time measurement */
  private[this] def wrap[A](function: => A): (A, Double) =
  {
    val evalStart = System.nanoTime()
    val result = function
    val evalEnd = System.nanoTime()
    (result, ((evalEnd-evalStart)/100).toDouble/10000)
  }

  def print(): Unit =
  {
    println(s"Result: ${tuple._1} | Time: ${tuple._2}ms")
  }
  def result: A = tuple._1
  def time: Double = tuple._2
}

object Wrapper
{
  def apply[A](function: => A): Wrapper[A] = new Wrapper[A](function)
}
