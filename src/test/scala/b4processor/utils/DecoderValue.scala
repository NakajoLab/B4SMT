package b4processor.utils

case class DecoderValue(
  valid: Boolean = false,
  source1: RVRegister = RVRegister(0),
  source2: RVRegister = RVRegister(0),
  destination: RVRegister = RVRegister(0),
  programCounter: Int = 0,
  isPrediction: Boolean = false
)
