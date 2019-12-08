import kamlib.Reader

val minimum = Reader.readString("/input8.txt").toList
  .map(_.asDigit).grouped(150).toList
  .map(x => (x.foldLeft(0){(acc, elem) => if (elem==0) acc+1 else acc}, x))
  .minBy(_._1)._2

val ones = minimum.foldLeft(0){(acc, next) => if (next==1) acc+1 else acc}
val twos = minimum.foldLeft(0){(acc, next) => if (next==2) acc+1 else acc}
println(ones*twos)

// =========== PART 2 =================== //

val array = Reader.readString("/input8.txt").toArray.map(_.asDigit)
val outputInts2 = Array.ofDim[Int](150)

@scala.annotation.tailrec
def determineColor(array: Array[Int], nextPointer: Int): Int =
{
  array(nextPointer) match
  {
    case 0 => 0
    case 1 => 1
    case 2 => determineColor(array, nextPointer+150)
  }
}

for(i<-0 until 150)
{
  outputInts2(i) = determineColor(array, i)
}

outputInts2.map(x => if (x == 1) 'X' else '.').grouped(25).foreach(x => println(x.mkString))
