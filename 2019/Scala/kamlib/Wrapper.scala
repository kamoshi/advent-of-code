package kamlib

/** Wraps a function, measures the execution time, provides the result and time*/
class Wrapper[A](function: => A)
{
  /** Saved results */
  val tuple: (A, Int) = wrap(function)

  /** Time measurement */
  private[this] def wrap[A](function: => A): (A, Int) =
  {
    val evalStart = System.currentTimeMillis()
    val result = function
    val evalEnd = System.currentTimeMillis()
    (result, (evalEnd-evalStart).toInt)
  }

  def print(): Unit =
  {
    println(s"Result: ${tuple._1} | Time: ${tuple._2}ms")
  }
  def result: A = tuple._1
  def time: Int = tuple._2
}

object Wrapper
{
  def apply[A](function: => A): Wrapper[A] = new Wrapper[A](function)
}
