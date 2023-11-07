package b4processor.utils

case class DecoderExpect(
  destinationTag: Int,
  sourceTag1: Option[Int],
  sourceTag2: Option[Int],
  value1: Option[Int],
  value2: Option[Int],
)
