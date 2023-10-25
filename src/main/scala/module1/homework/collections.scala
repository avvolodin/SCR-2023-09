package module1.homework

import scala.collection.immutable
import scala.util.Random

object collections1 {

  class Bucket(private var white: Int, private var black: Int){
    private def random = new Random()

    private def pull(): Boolean = {
      val r = random.nextInt(white + black) < white
      if(r)
        white = math.max(white - 1, 0)
      else
        black = math.max(black - 1, 0)
      r
    }

    def pullTwice: Boolean = pull || pull

  }

  def createCollection(number: Int, white: Int, black: Int): immutable.Seq[Bucket] =
    1 to number map(_ => new Bucket(white, black))

  def getResults(buckets: Seq[Bucket]): Seq[Boolean] = buckets.map(_.pullTwice)

  def getProbability(results: Seq[Boolean]): Double = (results.count(x => x).toDouble) / results.length

}
