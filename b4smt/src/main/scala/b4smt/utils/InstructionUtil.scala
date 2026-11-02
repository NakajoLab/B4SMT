package b4smt.utils

import chisel3._

import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using

object InstructionUtil {
  private def readLines(filename: String): Seq[String] =
    Using.resource(Source.fromFile(filename)) { file =>
      file.getLines().filter(_.nonEmpty).toVector
    }

  def fromFile32bit(filename: String): Seq[UInt] =
    fromStringSeq32bit(readLines(filename))

  def fromFile64bit(filename: String): Seq[UInt] =
    fromStringSeq64bit(readLines(filename))

  def fromStringSeq32bit(seq: Seq[String]): Seq[UInt] =
    fromStringSeq8bit(seq.flatMap { s =>
      val offset = if (s.startsWith("x")) 1 else 0
      (0 until 4).reverse.map(i => s.slice(i * 2 + offset, i * 2 + 2 + offset))
    })

  def fromStringSeq64bit(seq: Seq[String]): Seq[UInt] =
    fromStringSeq8bit(seq.flatMap { s =>
      val offset = if (s.startsWith("x")) 1 else 0
      (0 until 8).reverse.map(i => s.slice(i * 2 + offset, i * 2 + 2 + offset))
    })

  def fromBinaryFile(filename: String): Seq[UInt] =
    fromStringSeq8bit(
      Files
        .readAllBytes(Paths.get(filename))
        .iterator
        .map(b => f"${b & 0xff}%02X")
        .toVector,
    )

  def fromFile8bit(filename: String): Seq[UInt] =
    fromStringSeq8bit(readLines(filename))

  def fromStringSeq8bit(seq: Seq[String]): Seq[UInt] =
    seq
      .map { s =>
        val offset = if (s.startsWith("x")) 1 else 0
        s.slice(offset, offset + 2)
      }
      .grouped(8)
      .map(bytes => s"x${bytes.padTo(8, "00").reverse.mkString}".U(64.W))
      .toVector
}
