import module1.{hof, type_system}

object Main {

  def main(args: Array[String]): Unit = {

    val buckets = module1.homework.collections.createCollection(100000, 3, 3)
    val results = module1.homework.collections.getResults(buckets)
    val probability = module1.homework.collections.getProbability(results)

    println(probability)

  }
}