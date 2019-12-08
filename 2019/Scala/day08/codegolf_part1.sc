import kamlib.Reader
((list:List[Int])=>list.foldLeft(0){(acc,next)=>if(next==1)acc+1 else acc}*list.foldLeft(0){(acc, next)=>if(next==2)acc+1 else acc})(Reader.readString("/input8.txt").toList.map(_.asDigit).grouped(150).toList.map(x=>(x.foldLeft(0){(acc,elem)=>if(elem==0)acc+1 else acc},x)).minBy(_._1)._2)
