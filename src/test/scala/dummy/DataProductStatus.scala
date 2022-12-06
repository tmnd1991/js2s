sealed trait DataProductStatus extends Product with Serializable { val value: String }
object DataProductStatus {
  def valueOf(s: String): Option[DataProductStatus] = {
    s match {
      case Ghost.value =>
        Some(Ghost)
      case Mock.value =>
        Some(Mock)
      case Implemented.value =>
        Some(Implemented)
      case Deprecated.value =>
        Some(Deprecated)
      case To_be_implemented.value =>
        Some(To_be_implemented)
      case Deployed.value =>
        Some(Deployed)
      case Deleted.value =>
        Some(Deleted)
      case Created.value =>
        Some(Created)
      case _ =>
        None
    }
  }
  case object Ghost extends DataProductStatus { val value: String = "ghost" }
  case object Mock extends DataProductStatus { val value: String = "mock" }
  case object Implemented extends DataProductStatus { val value: String = "implemented" }
  case object Deprecated extends DataProductStatus { val value: String = "deprecated" }
  case object To_be_implemented extends DataProductStatus { val value: String = "to_be_implemented" }
  case object Deployed extends DataProductStatus { val value: String = "deployed" }
  case object Deleted extends DataProductStatus { val value: String = "deleted" }
  case object Created extends DataProductStatus { val value: String = "created" }
}
