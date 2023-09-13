import module1.{hof, type_system, list}

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello, World!")

    val l = List("fsadf","fasdf","fasd")
    val b = l.flatMap(_.toUpperCase)

    println("applyIter: " + module1.list.stat(()=>module1.list.testApply((a1, a2, a3, a4)=>module1.list.List.apply(a1,a2,a3,a4))))
    println("applyRec: " + module1.list.stat(()=>module1.list.testApply((a1, a2, a3, a4)=>module1.list.List.applyRec(a1,a2,a3,a4))))
    println("applyRecTail: " + module1.list.stat(()=>module1.list.testApply((a1, a2, a3, a4)=>module1.list.List.applyRecTail(a1,a2,a3,a4))))
//    applyIter: (1865.1, 29.539634391779458)
//    applyRec: (1944.8, 28.54575274887667)
//    applyRecTail: (2941.85, 43.531913580728336)
  }
}