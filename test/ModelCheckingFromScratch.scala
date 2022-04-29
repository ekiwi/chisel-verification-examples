import firrtl.backends.experimental.smt._
import org.scalatest.freespec.AnyFreeSpec

import chisel3._

/** Based on the SMT Solver Examples, this shows how simple BMC and induction works. */
class ModelCheckingFromScratch extends AnyFreeSpec {
  // this uses an example from the zipcpu slides:
  // https://zipcpu.com/tutorial/class-verilog.pdf (slide 99)
  "demonstrate bounded model checking with counter example" in {
    val (bv0, bv1, bv5, bv22, bv500) = (BVLiteral(0, 16), BVLiteral(1, 16), BVLiteral(5, 16), BVLiteral(22, 16), BVLiteral(500, 16))
    case class State(counter: BVSymbol, enable: BVSymbol)
    def makeState(num: Int): State = State(BVSymbol(s"counter_$num", 16), BVSymbol(s"enable_$num", 1))
    def init(s: State): BVExpr = BVEqual(s.counter, bv0)
    def next(s0: State, s1: State): BVExpr = {
      val nextCounter = BVIte(s0.enable,
        BVIte(BVEqual(s0.counter, bv22), bv0, BVOp(Op.Add, s0.counter, bv1)),
        s0.counter
      )
      // enable is an input and thus left unconstrained
      BVEqual(s1.counter, nextCounter)
    }
    def bad(s: State): BVExpr = {
      val assert0 = BVNot(BVEqual(s.counter, bv500))
      BVNot(assert0)
    }

    // manual bmc
    val (s0, s1) = (makeState(0), makeState(1))

    // check if the initial state violates the assertion
    assert(solve(Seq(init(s0), bad(s0))).isEmpty)
    // check if state 1 can violate the assertion
    assert(solve(Seq(init(s0), next(s0, s1), bad(s1))).isEmpty)

    // implement a bmc routine
    def bmc(k: Int): Unit = {
      var prev = makeState(0)
      var cond = List(init(prev))
      (0 to k).foreach { j =>
        val r = solve(cond :+ bad(prev))
        if(r.nonEmpty) {
          println(s"found bug after $j steps")
          println(r.toSeq.sorted.mkString("\n"))
          return
        }
        // add another state
        val state = makeState(j + 1)
        cond = cond :+ next(prev, state)
        prev = state
      }
    }

    bmc(10)

    // induction step w/o strengthening
    val ind0 = solve(Seq(BVNot(bad(s0)), next(s0, s1), bad(s1)))
    assert(ind0.nonEmpty, "without a strengthening invariant it should not pass")
    println(ind0)

    // induction step w/ strengthening
    def inv(s: State): BVExpr = BVComparison(Compare.GreaterEqual, bv22, s.counter, false)
    val check = solve(Seq(inv(s0), next(s0, s1), BVNot(inv(s1))))
    assert(check.isEmpty)
    val ind1 = solve(Seq(BVNot(bad(s0)), inv(s0), next(s0, s1), bad(s1)))
    assert(ind1.isEmpty)
  }
}

object failAfter {
  def apply(n: Int): Unit = {
    require(n > 0)
    val failCount = RegInit(n.U)
    failCount := failCount - 1.U
    assert(failCount =/= 0.U, s"failure triggered after $n cycles")
  }
}

class Counter extends Module {
  val en = IO(Input(Bool()))
  val count = RegInit(0.U(32.W))
  when(en) {
    when(count === 22.U) {
      count := 0.U
    }.otherwise {
      count := count + 1.U
    }
  }

  assert(count =/= 500.U)
}