import scala.util.Random

object Utils {
  def randomInt(rng:Random, n:Int):Int = {
    (rng.nextDouble() * n).toInt
  }

  // Generates an integer in [low, high) . 
  def randomInt(rng:Random, low:Int, high:Int) = {
    (rng.nextDouble() * (high - low)).toInt + low
  }

  def randomSelect[T](rng:Random, array:Array[T], k:Int):Array[T] = {
    def swap(i:Int, j:Int) = {
      val tmp = array(i)
      array(i) = array(j)
      array(j) = tmp
    }
    for (i <- 0 until k) {
      swap(i, randomInt(rng, i, array.length))
    }
    array.take(k)
  }

  case class VectorPimp[+T](x:Vector[T]) {
    def updateWith[U >: T](n:Int)(f:(T => U)):Vector[U] = {
      x.updated(n, f(x(n)))
    }
  }
  implicit def vectorToVectorPimp[T](x:Vector[T]) = VectorPimp(x) 
}