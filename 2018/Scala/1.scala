import scala.collection.mutable
import scala.io.Source
import scala.collection.immutable.LazyList.#::

val lines = Source.fromFile(getClass.getResource("/file.txt").getFile).getLines().toList

// Solution 1
val result1 = lines.foldLeft (0) { (acc, i) => acc + Integer.parseInt(i) }
println(result1)

// Solution 2
def toLazyLoop[A](list: List[A]): LazyList[A] = {
  def toLazy[A](list: List[A]): LazyList[A] = {
    list match {
      case List() => LazyList()
      case h::t => h #:: toLazy(t)
    }
  }
  toLazy(list) #::: toLazyLoop(list)
}

val hash = new mutable.HashSet[Int]()
def findFirstRepeat[A](lazylist:LazyList[String], acc:Int):Int =
  if (!hash.add(acc)) acc
  else lazylist match {
    case hd #:: tl => findFirstRepeat(tl, (acc + hd.toInt))
  }

val result2 = findFirstRepeat(toLazyLoop(lines), 0)
println("First repeat frequency: " + result2)
