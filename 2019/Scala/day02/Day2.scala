package day02

class Day2 {

  /** Find solution for part1 */
  def solveP1(arr: Array[Int]): Int =
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

  /** Find fuel needed and recursive fuel */
  def solveP2(arr:Array[Int]):(Int, Int) =
  {
    var tuple = (0, 0)
    for (i <- 0 to 99; j <- 0 to 99)
    {
      val copy = arr.clone()
      copy(1) = i
      copy(2) = j
      if (solveP1(copy) == 19690720) tuple = (i, j)
    }
    tuple
  }

}
