import scala.io.Source

var original = Source.fromFile(getClass.getResource("/input2.txt").getFile).mkString.split("\\D+").map(x => x.toInt)
original(1) = 12
original(2) = 2
var copy1 = original.clone()

// 1
def tapeTraversal1(arr:Array[Int]):Int =
{
  var i = 0
  while (arr(i) == 1 || arr(i) == 2)
  {
    arr(i) match {
      case 1 => arr(arr(i + 3)) = arr(arr(i + 1)) + arr(arr(i + 2)); i += 4
      case 2 => arr(arr(i + 3)) = arr(arr(i + 1)) * arr(arr(i + 2)); i += 4
      case _ => ()
    }
  }
  arr(0)
}
val result1 = tapeTraversal1(copy1)

// 2
def bruteForce2(arr:Array[Int]):(Int, Int) =
{
  var tuple = (0, 0)
  for (i <- 0 to 99; j <- 0 to 99)
  {
    var copy = arr.clone()
    copy(1) = i
    copy(2) = j
    if (tapeTraversal1(copy) == 19690720) tuple = (i, j)
  }
  tuple
}

val result2 = bruteForce2(original)





