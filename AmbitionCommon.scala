object AmbitionCommon {
  case class RoundResult(pointsTaken:Vector[Int],
                         slams:Vector[Boolean],
                         nils:Vector[Boolean],
                         understrikes:Vector[Boolean],
                         overstrikes:Vector[Boolean],
                         nilstrikes:Vector[Boolean],
                         pointsScored:Vector[Int],
                         strikes:Vector[Boolean])

  case class AllScores(strikes:Vector[Int], points:Vector[Int])
}
