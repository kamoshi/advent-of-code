import kamlib.Reader

val zeroes = Reader.readString("/input8.txt").toList.map(_.asDigit).grouped(150).toList.map(x => (x.foldLeft(0){(acc, elem) => if (elem==0) acc+1 else acc}, x))
val minimum = zeroes.minBy(_._1)._2

val ones = minimum.foldLeft(0){(acc, next) => if (next==1) acc+1 else acc}
val twos = minimum.foldLeft(0){(acc, next) => if (next==2) acc+1 else acc}
println(ones*twos)
