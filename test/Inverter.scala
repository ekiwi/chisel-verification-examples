import chisel3._
import chisel3.experimental.verification
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec



class InverterWithFormalSpec extends Module {
  val in = IO(Input(Bool()))
  val out = IO(Output(Bool()))
  val hold = IO(Input(Bool()))

  val delay = RegInit(false.B)
  when(!hold) {
    delay := !in
  }
  out := delay

  when(past(hold)) {
    verification.assert(stable(out))
  }.otherwise {
    verification.assert(out === !past(in))
  }
}

class InverterWithDefectiveFormalSpec extends Module {
  val in = IO(Input(Bool()))
  val out = IO(Output(Bool()))
  val hold = IO(Input(Bool()))

  val delay = RegInit(false.B)
  when(!hold) {
    delay := !in
  }
  out := delay

  when(RegNext(hold)) {
    verification.assert(out === RegNext(out))
  }.otherwise {
    val inPast = dontTouch(RegNext(in)).suggestName("inPast")
    verification.assert(out === !inPast)
  }
}


class InverterTest extends AnyFlatSpec with ChiselScalatestTester with Formal {
  behavior of "Inverter"

  it should "fail the test if we set hold=true" in {
    // you can view the VCD by running
    // > gtkwave test_run_dir/Inverter_should_fail_the_test_if_we_set_holdtrue/Inverter.vcd

    test(new Inverter).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.in.poke(true.B)
      dut.hold.poke(false.B)
      dut.clock.step()
      dut.out.expect(false.B)

      dut.in.poke(false.B)
      dut.hold.poke(true.B)
      dut.clock.step()
      dut.out.expect(false.B)
    }
  }

  it should "pass the test if we set hold=false" in {
    test(new Inverter) { dut =>
      dut.in.poke(true.B)
      dut.hold.poke(false.B)
      dut.clock.step()
      dut.out.expect(false.B)
    }
  }

  it should "pass a bounded check when using past" in {
    verify(new InverterWithFormalSpec, Seq(BoundedCheck(10)))
  }


  it should "fail a bounded check when using RegNext" in {
    // you can view the VCD by running
    // > gtkwave test_run_dir/Inverter_should_fail_a_bounded_check_when_using_RegNext/InverterWithDefectiveFormalSpec.vcd
    assertThrows[FailedBoundedCheckException](verify(new InverterWithDefectiveFormalSpec, Seq(BoundedCheck(10))))
  }
}
