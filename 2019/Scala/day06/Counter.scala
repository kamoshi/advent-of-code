package day06

class Counter
{
  private[this] var counter: Int = 0
  def count: Int = counter
  def inc(n: Int): Unit = counter += n
}
