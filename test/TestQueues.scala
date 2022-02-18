import chisel3._
import chisel3.experimental.IO
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec



class TestQueues extends AnyFlatSpec with ChiselScalatestTester with Formal {
  behavior of "Queues"

  it should "verify QueueV0" in {
    verify(new QueueFormalTest(new MyQueueV0(32)), Seq(BoundedCheck(10), BtormcEngineAnnotation))
  }

}

class QueueFormalTest(makeQueue: => IsQueue) extends Module {
  val dut = Module(makeQueue)
  val io = IO(chiselTypeOf(dut.io))
  io <> dut.io
  MagicPacketTracker(dut.io.enq, dut.io.deq, dut.numEntries)
}


///////////////////////////////////////
// Magic Packet Tracker
//////////////////////////////////////

object MagicPacketTracker {
  def apply[D <: Data](enq: DecoupledIO[D], deq: DecoupledIO[D], depth: Int): Unit = {
    val tracker = Module(new MagicPacketTracker(chiselTypeOf(enq.bits), depth))
    tracker.enq := enq
    tracker.deq := deq
    val startTracking = IO(Input(Bool()))
    tracker.startTracking := startTracking
  }
}

/** Tracks random packets for formally verifying FIFOs
 *
 *  This ensures that when some data enters the FIFO, it
 *  will always be dequeued after the correct number of
 *  elements.
 *  So essentially we are verifying data integrity.
 *  Note that this does not imply that the FIFO has no bugs
 *  since e.g., a FIFO that never allows elements to be
 *  enqueued would easily pass our assertions.
 */
class MagicPacketTracker[D <: Data](dataTpe: D, fifoDepth: Int) extends Module {
  val enq = IO(Input(DecoupledIO(dataTpe)))
  val deq = IO(Input(DecoupledIO(dataTpe)))

  // count the number of elements in the fifo
  val elementCount = RegInit(0.U(log2Ceil(fifoDepth + 1).W))
  val nextElementCount = Mux(
    enq.fire && !deq.fire,
    elementCount + 1.U,
    Mux(!enq.fire && deq.fire, elementCount - 1.U, elementCount)
  )
  elementCount := nextElementCount

  // track a random "magic" packet through the fifo
  val startTracking = IO(Input(Bool()))
  val isActive = RegInit(false.B)
  val packetValue = Reg(chiselTypeOf(enq.bits))
  val packetCount = Reg(chiselTypeOf(elementCount))

  when(!isActive && enq.fire && startTracking) {
    when(deq.fire && elementCount === 0.U) {
      assert(
        enq.bits.asUInt === deq.bits.asUInt,
        "element should pass through the fifo, but %x != %x",
        packetValue.asUInt,
        deq.bits.asUInt
      )
    }.otherwise {
      isActive := true.B
      packetValue := enq.bits
      packetCount := nextElementCount
    }
  }

  when(isActive && deq.fire) {
    packetCount := packetCount - 1.U
    when(packetCount === 1.U) {
      assert(
        packetValue.asUInt === deq.bits.asUInt,
        "element should be dequeued in this cycle, but %x != %x",
        packetValue.asUInt,
        deq.bits.asUInt
      )
      isActive := false.B
    }
  }
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
  when (indicesEqual && io.deq.fire =/= io.enq.fire) {
    maybeFull := !maybeFull
  }
  when (io.deq.fire) {
    deqIndex.inc()
  }
  when (io.enq.fire) {
    entries(enqIndex.value) := io.enq.bits
    enqIndex.inc()
  }
}

