import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec



class TestQueues extends AnyFlatSpec with ChiselScalatestTester with Formal {
  behavior of "Queues"

  val DefaultDepth = 3
  val DefaultDepthPow2 = 4 // some queues only work with power of two
  val DefaultBmc = 7

  it should "verify QueueV0" in {
    verify(new QueueFormalTest(new MyQueueV0(32)), Seq(BoundedCheck(DefaultBmc), BtormcEngineAnnotation))
  }

  it should "verify QueueV1" in {
    verify(new QueueFormalTest(new MyQueueV1(DefaultDepth,32)), Seq(BoundedCheck(DefaultBmc), BtormcEngineAnnotation))
  }

  it should "verify QueueV2" ignore { // TODO: QueueV2 is broken
    verify(new QueueFormalTest(new MyQueueV2(DefaultDepth,32)), Seq(BoundedCheck(DefaultBmc), BtormcEngineAnnotation))
  }

  it should "verify QueueV3" in {
    verify(new QueueFormalTest(new MyQueueV3(DefaultDepthPow2,32)), Seq(BoundedCheck(DefaultBmc), BtormcEngineAnnotation))
  }

  it should "verify QueueV4" in {
    verify(new QueueFormalTest(new MyQueueV4(DefaultDepthPow2,32)), Seq(BoundedCheck(DefaultBmc), BtormcEngineAnnotation))
  }

  it should "verify QueueV5" in {
    verify(new QueueFormalTest(new MyQueueV5(DefaultDepthPow2,32)), Seq(BoundedCheck(DefaultBmc), BtormcEngineAnnotation))
  }

  it should "verify QueueV6 w/ pipe = false" in {
    verify(new QueueFormalTest(new MyQueueV6(DefaultDepth,32, pipe = false)), Seq(BoundedCheck(DefaultBmc), BtormcEngineAnnotation))
  }

  it should "verify QueueV6 w/ pipe = true" in {
    verify(new QueueFormalTest(new MyQueueV6(DefaultDepth,32, pipe = true)), Seq(BoundedCheck(DefaultBmc), BtormcEngineAnnotation))
  }
}

class QueueFormalTest(makeQueue: => IsQueue) extends Module {
  val dut = Module(makeQueue)
  val io = IO(chiselTypeOf(dut.io))
  io <> dut.io
  MagicPacketTracker(dut.io.enq, dut.io.deq, dut.numEntries, debugPrint = true)
}

///////////////////////////////////////////////////////
// Queues from Scott Beamer's agile hardware lectures
///////////////////////////////////////////////////////

trait IsQueue extends Module {
  def io: QueueIO
  def numEntries: Int
}

class QueueIO(bitWidth: Int) extends Bundle {
  val enq = Flipped(Decoupled(UInt(bitWidth.W)))
  val deq = Decoupled(UInt(bitWidth.W))
}

class MyQueueV0(bitWidth: Int) extends Module with IsQueue {
  override val numEntries = 1
  val io = IO(new QueueIO(bitWidth))
  val entry = Reg(UInt(bitWidth.W))
  val full = RegInit(false.B)
  io.enq.ready := !full || io.deq.fire
  io.deq.valid := full
  io.deq.bits := entry
  when (io.deq.fire) {
    full := false.B
  }
  when (io.enq.fire) {
    entry := io.enq.bits
    full := true.B
  }
}

class MyQueueV1(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 0)
  // enqueue into index numEntries-1 (last) and dequeue from index 0 (head)
  val entries = Seq.fill(numEntries)(Reg(UInt(bitWidth.W)))
  val fullBits = Seq.fill(numEntries)(RegInit(false.B))
  val shiftDown = io.deq.fire || !fullBits.head
  io.enq.ready := !fullBits.last || shiftDown
  io.deq.valid := fullBits.head
  io.deq.bits := entries.head
  when(shiftDown) { // dequeue / shift
    for (i <- 0 until numEntries - 1) {
      entries(i) := entries(i + 1)
      fullBits(i) := fullBits(i + 1)
    }
    fullBits.last := false.B
  }
  when(io.enq.fire) { // enqueue
    entries.last := io.enq.bits
    fullBits.last := true.B
  }
  //     when (shiftDown || io.enq.fire) {
  //         entries.foldRight(io.enq.bits){(thisEntry, lastEntry) => thisEntry := lastEntry; thisEntry}
  //         fullBits.foldRight(io.enq.fire){(thisEntry, lastEntry) => thisEntry := lastEntry; thisEntry}
  //
}


class MyQueueV2(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 0)
  // enqueue into lowest empty and dequeue from index 0 (head)
  val entries = Reg(Vec(numEntries, UInt(bitWidth.W)))
  val fullBits = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val emptyBits = fullBits map { !_ }
  io.enq.ready := emptyBits reduce { _ || _ } // any empties?
  io.deq.valid := fullBits.head
  io.deq.bits := entries.head
  when (io.deq.fire) { // dequeue & shift up
    for (i <- 0 until numEntries - 1) {
      entries(i) := entries(i+1)
      fullBits(i) := fullBits(i+1)
    }
    fullBits.last := false.B
  }
  when (io.enq.fire) { // priority enqueue
    val writeIndex = PriorityEncoder(emptyBits)
    entries(writeIndex) := io.enq.bits
    fullBits(writeIndex) := true.B
  }
}

class MyQueueV3(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 1)
  require(isPow2(numEntries))
  val entries = Reg(Vec(numEntries, UInt(bitWidth.W))) // Mem?
  val enqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val deqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val empty = enqIndex === deqIndex
  val full = (enqIndex +% 1.U) === deqIndex
  io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := entries(deqIndex)
  when (io.deq.fire) {
    deqIndex := deqIndex +% 1.U
  }
  when (io.enq.fire) {
    entries(enqIndex) := io.enq.bits
    enqIndex := enqIndex +% 1.U
  }
}

class MyQueueV4(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 1)
  require(isPow2(numEntries))
  val entries = Reg(Vec(numEntries, UInt(bitWidth.W)))
  val enqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val deqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val maybeFull = RegInit(false.B)
  val empty = enqIndex === deqIndex && !maybeFull
  val full = enqIndex === deqIndex && maybeFull
  io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := entries(deqIndex)
  when (io.deq.fire) {
    deqIndex := deqIndex +% 1.U
    when (enqIndex =/= deqIndex) {
      maybeFull := false.B
    }
  }
  when (io.enq.fire) {
    entries(enqIndex) := io.enq.bits
    enqIndex := enqIndex +% 1.U
    when ((enqIndex +% 1.U) === deqIndex) {
      maybeFull := true.B
    }
  }
}

class MyQueueV5(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 1)
  require(isPow2(numEntries))
  val entries = Reg(Vec(numEntries, UInt(bitWidth.W)))
  val enqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val deqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val maybeFull = RegInit(false.B)
  val empty = enqIndex === deqIndex && !maybeFull
  val full = enqIndex === deqIndex && maybeFull
  io.enq.ready := !full || io.deq.ready  // NOTE: io.enq.ready now attached to io.deq.ready
  io.deq.valid := !empty
  io.deq.bits := entries(deqIndex)
  when (io.deq.fire) {
    deqIndex := deqIndex +% 1.U
    when (enqIndex =/= deqIndex) {
      maybeFull := false.B
    }
  }
  when (io.enq.fire) {
    entries(enqIndex) := io.enq.bits
    enqIndex := enqIndex +% 1.U
    when ((enqIndex +% 1.U) === deqIndex) {
      maybeFull := true.B
    }
  }
}

class MyQueueV6(val numEntries: Int, bitWidth: Int, pipe: Boolean=true) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 1)
  //     require(isPow2(numEntries))    // no longer needed
  val entries = Mem(numEntries, UInt(bitWidth.W))
  val enqIndex = Counter(numEntries)
  val deqIndex = Counter(numEntries)
  val maybeFull = RegInit(false.B)
  val indicesEqual = enqIndex.value === deqIndex.value
  val empty = indicesEqual && !maybeFull
  val full = indicesEqual && maybeFull
  if (pipe)
    io.enq.ready := !full || io.deq.ready
  else
    io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := entries(deqIndex.value)
  val fixed = true
  if(fixed) {
    when (io.deq.fire =/= io.enq.fire) {
      maybeFull := io.enq.fire
    }
  } else {
    when (indicesEqual && io.deq.fire =/= io.enq.fire) {
      maybeFull := !maybeFull
    }
  }
  when (io.deq.fire) {
    deqIndex.inc()
  }
  when (io.enq.fire) {
    entries(enqIndex.value) := io.enq.bits
    enqIndex.inc()
  }
}

