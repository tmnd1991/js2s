sealed trait Protocol extends Product with Serializable { val value: String }
object Protocol {
  def valueOf(s: String): Option[Protocol] = {
    s match {
      case Http.value =>
        Some(Http)
      case Https.value =>
        Some(Https)
      case _ =>
        None
    }
  }
  case object Http extends Protocol { val value: String = "http" }
  case object Https extends Protocol { val value: String = "https" }
}
