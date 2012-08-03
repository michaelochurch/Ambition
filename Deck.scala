import scala.util.Random

object Deck {
  import CardUtils._

  val full = for (s <- Suit.values.toList; r <- Rank.values.toList) yield Card(r, s)

  def shuffled(rng:Random):Seq[Card] = rng.shuffle(full)

  def deal(rng:Random):Vector[Seq[Card]] = {
    val s = shuffled(rng)
    Vector(s.slice(0, 13), s.slice(13, 26), s.slice(26, 39), s.slice(39, 52))
  }
}
