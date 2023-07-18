package b4processor.utils.axi

import chisel3._

class VerilogAXI(
  val dataWidth: Int,
  val addressWidth: Int,
  val idWidth: Int = 0,
  val userWriteWidth: Int = 0,
  val userWriteAddressWidth: Int = 0,
  val userWriteResponseWidth: Int = 0,
  val userReadAddressWidth: Int = 0,
  val userReadWidth: Int = 0
) extends Bundle {
  val awid = Output(UInt(idWidth.W))
  val awaddr = Output(UInt(addressWidth.W))
  val awlen = Output(UInt(8.W))
  val awsize = Output(BurstSize())
  val awburst = Output(BurstType())
  val awlock = Output(Lock())
  val awcache = Output(UInt(4.W))
  val awprot = Output(UInt(3.W))
  val awqos = Output(UInt(4.W))
  val awuser = Output(UInt(userWriteAddressWidth.W))
  val awvalid = Output(Bool())
  val awready = Input(Bool())
  val wdata = Output(UInt(dataWidth.W))
  val wstrb = Output(UInt((dataWidth / 8).W))
  val wlast = Output(Bool())
  val wuser = Output(UInt(userWriteWidth.W))
  val wvalid = Output(Bool())
  val wready = Input(Bool())
  val bid = Input(UInt(idWidth.W))
  val bresp = Input(Response())
  val buser = Input(UInt(userWriteResponseWidth.W))
  val bvalid = Input(Bool())
  val bready = Output(Bool())
  val arid = Output(UInt(idWidth.W))
  val araddr = Output(UInt(addressWidth.W))
  val arlen = Output(UInt(8.W))
  val arsize = Output(BurstSize())
  val arburst = Output(BurstType())
  val arlock = Output(Lock())
  val arcache = Output(UInt(4.W))
  val arprot = Output(UInt(3.W))
  val arqos = Output(UInt(4.W))
  val aruser = Output(UInt(userReadAddressWidth.W))
  val arvalid = Output(Bool())
  val arready = Input(Bool())
  val rid = Input(UInt(idWidth.W))
  val rdata = Input(UInt(dataWidth.W))
  val rresp = Input(Response())
  val rlast = Input(Bool())
  val ruser = Input(UInt(userReadWidth.W))
  val rvalid = Input(Bool())
  val rready = Output(Bool())
}
