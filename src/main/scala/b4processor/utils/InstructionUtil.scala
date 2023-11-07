package b4processor.utils

import chisel3._

import java.nio.file.{Files, Paths}
import scala.io.Source

object InstructionUtil {
  def fromFile32bit(filename: String): Seq[UInt] = {
    val file = Source.fromFile(filename)
    val output = fromStringSeq32bit(file.getLines().filter(_.nonEmpty).toSeq)
    file.close()
    output
  }

  def fromFile64bit(filename: String): Seq[UInt] = {
    val file = Source.fromFile(filename)
    val output = fromStringSeq64bit(file.getLines().filter(_.nonEmpty).toSeq)
    file.close()
    output
  }

  def fromStringSeq32bit(seq: Seq[String]): Seq[UInt] = {
    var bytes = Seq[String]()
    for (s <- seq) {
      var offset = 0
      if (s.startsWith("x"))
        offset = 1
      for (i <- (0 until 4).reverse)
        bytes = bytes :+ s.slice(i * 2 + offset, i * 2 + 2 + offset)
    }
    fromStringSeq8bit(bytes)
  }

  def fromStringSeq64bit(seq: Seq[String]): Seq[UInt] = {
    var bytes = Seq[String]()
    for (s <- seq) {
      var offset = 0
      if (s.startsWith("x"))
        offset = 1
      for (i <- (0 until 8).reverse)
        bytes = bytes :+ s.slice(i * 2 + offset, i * 2 + 2 + offset)
    }
    fromStringSeq8bit(bytes)
  }

  def fromBinaryFile(filename: String): Seq[UInt] = {
    val bytes = Files.readAllBytes(Paths.get(filename))
    val s = bytes.map("%02X" format _).toSeq
    val output = fromStringSeq8bit(s)
    output
  }

  def fromFile8bit(filename: String): Seq[UInt] = {
    val file = Source.fromFile(filename)
    val output = fromStringSeq8bit(file.getLines().filter(_.nonEmpty).toSeq)
    file.close()
    output
  }

  def fromStringSeq8bit(seq: Seq[String]): Seq[UInt] = {
    var output = Seq[String]()
    for (s <- seq) {
      var offset = 0
      if (s.startsWith("x"))
        offset = 1
      output = output :+ s.slice(offset, offset + 2)
    }
    var output64 = Seq[String]()
    for (i <- 0 until math.ceil(output.length.toDouble / 8).toInt) {
      output64 = output64 :+ (0 until 8)
        .map(k => i * 8 + k)
        .map(p =>
          if (p < output.length) { output(p) }
          else { "00" },
        )
        .reverse
        .reduce(_ + _)
    }
    output64.map(n => s"x$n".U(64.W))
  }
}
