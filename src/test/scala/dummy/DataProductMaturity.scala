sealed trait DataProductMaturity extends Product with Serializable { val value: String }
object DataProductMaturity {
  def valueOf(s: String): Option[DataProductMaturity] = {
    s match {
      case Tactical.value =>
        Some(Tactical)
      case Strategy.value =>
        Some(Strategy)
      case _ =>
        None
    }
  }
  case object Tactical extends DataProductMaturity { val value: String = "Tactical" }
  case object Strategy extends DataProductMaturity { val value: String = "Strategy" }
}
