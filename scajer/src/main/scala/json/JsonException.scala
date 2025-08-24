package scajer.json

case class JsonException(msg: String) extends RuntimeException(msg) {
  override def fillInStackTrace(): Throwable = this
}
