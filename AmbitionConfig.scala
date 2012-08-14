sealed trait ScoringSystem
case object OldSystem extends ScoringSystem
case object SimpleSystem extends ScoringSystem

case class AmbitionConfig(
  val scoringSystem : ScoringSystem
)

object AmbitionConfig {
  var it = AmbitionConfig(scoringSystem = SimpleSystem)
}
