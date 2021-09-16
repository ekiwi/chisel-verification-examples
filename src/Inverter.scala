import chisel3._


class Inverter extends Module {
  val in = IO(Input(Bool()))
  val out = IO(Output(Bool()))
  val hold = IO(Input(Bool()))

  val delay = Reg(Bool())
  when(!hold) {
    delay := !in
  }
  out := delay
}


class InverterIO extends Bundle {
  val in = Input(Bool())
  val out = Output(Bool())
  val hold = Input(Bool())
}

class InverterWihtBundle extends Module {
  val io = IO(new InverterIO)

  val delay = Reg(Bool())
  when(!io.hold) {
    delay := !io.in
  }
  io.out := delay
}

class InverterWrap extends Module {
  val io = IO(new InverterIO)
  val inv = Module(new InverterWihtBundle)
  io <> inv.io
}

class InverterWithParameter(ignoreHold: Boolean) extends Module {
  val in = IO(Input(Bool()))
  val out = IO(Output(Bool()))
  val hold = IO(Input(Bool()))

  val delay = Reg(Bool())
  if(ignoreHold) {
    delay := !in
  } else {
    when(!hold) {
      delay := !in
    }
  }
  out := delay
}
